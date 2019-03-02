(ns clojush.interpreter
  (:use [clojush pushstate globals util]
        [clojush.instructions tag input-output]
        [clojush.experimental.tagged-code-macros])
  (:require [clojure.string :as str]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; push interpreter



(defn attach-ids
  [orig ids]
  (loop [x 0
       result '()]
    (if (= x (count orig))
      (reverse result)
      (recur (inc x) (let [elem (nth orig x)
                           elem-id (try 
                                     (nth ids x)
                                     (catch Exception e (str "caught exception: " (.getMessage e) elem)))]
                       
                         (if (seq? elem)
                           (conj result (attach-ids elem elem-id))
                           (conj result (vector elem elem-id)))
                       
)
                       ))))

(defn remove-ids
  "The secoond argument decides whether you want exec stack (0) or the idenifiers (1)."
  [comb y]
  (loop [x 0
       result '()]
    (if (= x (count comb))
      (reverse result)
      (recur (inc x) (let [elem (nth comb x)
                           ]
                       (if (seq? elem)
                         (conj result (remove-ids elem y))
                         (conj result (nth elem y))))))))


(defn correct-ids
  "Sometimes the instructions can call some other instruction that has not been assinged an id.
  This function assigns an id to suc instructions. For example, exec_do*while calls exec_while."
  [comb exec-ids]
  (doall (map (fn [x] (if (vector? x)
                        x
                        (if (seq? x)
                          (correct-ids x exec-ids)
                          (vector x (inc (apply max (flatten exec-ids))))))) comb)))
  

(defn execute-instruction
  "Executes a single Push instruction."
  [instruction state]
  ;; for debugging only, e.g. for stress-test
  ;(def debug-recent-instructions (cons instruction debug-recent-instructions))
  ;(def debug-recent-state state)
  (swap! point-evaluations-count inc)
  (if (= instruction nil) ;; tests for nil and ignores it
    state
    (let [literal-type (recognize-literal instruction)
          ;_ (prn "Attaching process: ")
          ;_ (prn (:exec state))
          ;_ (prn (:exec_id state))
          ;state (assoc state :exec (attach-ids (:exec state) (:exec_id state)))
          ]
      (cond
        ;
        literal-type 
        (push-item instruction literal-type state)
        ;
        (and (vector? instruction) (= [] instruction)) 
        (push-item [] :vector_integer 
                   (push-item [] :vector_float 
                              (push-item [] :vector_string 
                                         (push-item [] :vector_boolean state))))
        ;
        (and (symbol? instruction) 
             (re-seq #"in\d+" (name instruction))) 
        (handle-input-instruction instruction state)
        ;
        (tag-instruction? instruction) 
        (handle-tag-instruction instruction state)
        ;
        (tagged-code-macro? instruction) 
        (handle-tag-code-macro instruction state)
        ;
        ; Attach identifiers only when the instruction starts with exec_.
        (contains? @instruction-table instruction)  
        (if (and (str/starts-with? (str instruction) "exec_") @global-calculate-mod-metrics)
          ((instruction @instruction-table) (assoc state :exec (attach-ids (:exec state) (:exec_id state))))
          ((instruction @instruction-table) state)
        )
        ;
        ;
        ;(contains? @instruction-table instruction)  
        ;((instruction @instruction-table) state)
        ;
        :else 
        (throw (Exception. (str "Undefined instruction: " (pr-str instruction))))))))

(def saved-state-sequence (atom []))

(defn eval-push 
  "Executes the contents of the exec stack, aborting prematurely if execution limits are 
   exceeded. The resulting push state will map :termination to :normal if termination was 
   normal, or :abnormal otherwise."
  ([state] (eval-push state false false false))
  ([state print-steps] (eval-push state print-steps false false))
  ([state print-steps trace] (eval-push state print-steps trace false))
  ([state print-steps trace save-state-sequence]
    ;(when (empty? @global-atom-generators)
    ;  (println "global-atom-generators is empty. You should do something like: (reset! global-atom-generators '(exec_if boolean_not true false))"))
   (loop [iteration 1
          s state
          time-limit (if (zero? @global-evalpush-time-limit)
                       0
                       (+' @global-evalpush-time-limit (System/nanoTime)))]
     (if (or (> iteration @global-evalpush-limit)
             (and (empty? (:exec s)) (empty? (:environment s)))
             (and (not (zero? time-limit))
                  (> (System/nanoTime) time-limit)))
       (assoc s :termination (if (and (empty? (:exec s)) (empty? (:environment s)))
                               :normal
                               :abnormal))
       (if (empty? (:exec s))
         (let [s (end-environment s)]
           (when print-steps
             (printf "\nState after %s steps (last step: %s):\n" 
                     iteration "end_environment_from_empty_exec")
             (state-pretty-print s))
           (when save-state-sequence
             (swap! saved-state-sequence #(conj % s)))
           (recur (inc iteration) s time-limit))
         (let [exec-top (top-item :exec s)
               exec-top-id (if @global-calculate-mod-metrics
                             (top-item :exec_id s)
                             nil)
               s (pop-item :exec s)
               s (if @global-calculate-mod-metrics
                   (pop-item :exec_id s)
                   s)
               ]
           (let [s (if (seq? exec-top)
                     (let [new-s (assoc s :exec (concat exec-top (:exec s)))
                           new-s (assoc new-s :exec_id (concat exec-top-id (:exec_id new-s)))]
                       (cond
                         (= trace false) new-s
                         (= trace true) (let [new-s (assoc new-s
                                                           :trace
                                                           (cons exec-top (let [t (:trace s)] (if (seq? t) t ()))))]
                                          (if @global-calculate-mod-metrics
                                            (assoc new-s
                                                   :trace_id
                                                   (cons exec-top-id (let [t (:trace_id s)] (if (seq? t) t ()))))))                                       
                         
                         ))
                     
                     (let [execution-result (execute-instruction exec-top s)
                            ; Need to use remove-ids() only when the instruction was exec related. Refer execute-instructions for more details.
                           execution-result (if (and (str/starts-with? (str exec-top) "exec_") @global-calculate-mod-metrics)
                                              (let [ execution-result (assoc execution-result :exec (correct-ids (:exec execution-result) (:exec_id execution-result)))
                                                     ; do a quick scan of all the instr:id pairs. if id is missing for any instr, give it a new id.
                                                    execution-result (assoc execution-result :exec_id (remove-ids (:exec execution-result) 1))
                                                    execution-result (assoc execution-result :exec (remove-ids (:exec execution-result) 0))]
                                                execution-result
                                                )
                                              execution-result
                                              )
                           ]
                       (cond
                          ;(= trace false) execution-result
                         (or (= trace true) (= trace false)) (let [execution-result (assoc execution-result
                                                                                           :trace
                                                                                           (cons exec-top (let [t (:trace s)] (if (seq? t) t ()))))]
                                                               (if @global-calculate-mod-metrics
                                                                 (assoc execution-result
                                                                        :trace_id
                                                                        (cons exec-top-id (let [t (:trace_id s)] (if (seq? t) t ()))))))
                         (= trace :changes) (if (= execution-result s)
                                              execution-result
                                              (let [execution-result (assoc execution-result
                                                                            :trace
                                                                            (cons exec-top (let [t (:trace s)] (if (seq? t) t ()))))]
                                                (if @global-calculate-mod-metrics
                                                  (assoc execution-result
                                                         :trace_id
                                                         (cons exec-top-id (let [t (:trace_id s)] (if (seq? t) t ()))))))))))]
             (when print-steps
               (printf "\nState after %s steps (last step: %s):\n"
                       iteration (if (seq? exec-top) "(...)" exec-top))
               (state-pretty-print s))
             (when trace
               (prn ":trace in correct order = " (reverse (:trace s))))
             (when trace
               (prn ":trace-id in correct order = " (reverse (:trace_id s))))
             (when save-state-sequence
               (swap! saved-state-sequence #(conj % s)))
             (recur (inc iteration) s time-limit))))))))

(defn assign-ids
  "Assign idenitfiers (starts from 0) to the instruction in the expression. Retains the braces. Just replaces the instructions
   with their idenifiers."
  [expr init-id]
  (loop [x 0 ;index
           y init-id ;id
           result '()]
      (if (= x (count expr))
        (reverse result)
        (recur (inc x) (let [_ (swap! y inc)]
                         y) 
               (let [elem (nth expr x)]
                         (if (seq? elem)
                         (conj result (assign-ids elem (let [_ (swap! y dec)]
                         y) ))
                           (conj result @y))
        )))))


(defn run-push 
  "The top level of the push interpreter; calls eval-push between appropriate code/exec 
   pushing/popping. The resulting push state will map :termination to :normal if termination was 
   normal, or :abnormal otherwise."
  ([code state]
   (run-push code state false false false))
  ([code state print-steps]
   (run-push code state print-steps false false))
  ([code state print-steps trace]
   (run-push code state print-steps trace false))
  ([code state print-steps trace save-state-sequence]
   (let [s (if @global-top-level-push-code (push-item code :code state) state)]
     (let [s (push-item (not-lazy code) :exec s)
          ;; If calculate-mod-metrics if true, do the followiing
           s (if @global-calculate-mod-metrics
               (push-item (assign-ids (not-lazy code) (atom -1)) :exec_id s)
               s)
           ]
       (when print-steps
         (printf "\nState after 0 steps:\n")
         (state-pretty-print s))
       (when save-state-sequence
         (reset! saved-state-sequence [s]))
       (let [s (eval-push s print-steps trace save-state-sequence)]
         (if @global-top-level-pop-code
           (pop-item :code s)
           s))))))


