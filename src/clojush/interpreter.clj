(ns clojush.interpreter
  (:use [clojush pushstate globals util mod_metrics]
        [clojush.instructions tag input-output]
        [clojush.experimental.tagged-code-macros])
  (:require [clojure.string :as str]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; push interpreter


(defn execute-instruction
  "Executes a single Push instruction."
  [instruction state]
  ;; for debugging only, e.g. for stress-test
  ;(def debug-recent-instructions (cons instruction debug-recent-instructions))
  ;(def debug-recent-state state)
  (swap! point-evaluations-count inc)
  (if (= instruction nil) ;; tests for nil and ignores it
    state
    (let [literal-type (recognize-literal instruction)]
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
        (contains? @instruction-table instruction)
        ((instruction @instruction-table) state)
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
               s (pop-item :exec s)]
           (let [s (let [exec-result (if (seq? exec-top)
                                       (assoc s :exec (concat exec-top (:exec s)))
                                       (if (:calculate-mod-metrics state) ; additional operations only when calculating mod metrics
                                         (let [result (execute-instruction (:instr exec-top) s)]
                                           (assoc result :exec (correct-ids (:exec result) (atom 1000000))))
                                         (execute-instruction exec-top s)
                                         ))]
                     (cond
                       (and (= trace false) (not (:calculate-mod-metrics state))) exec-result
                       (or (= trace true) (:calculate-mod-metrics state)) (assoc exec-result
                                                                            :trace
                                                                            (cons exec-top (let [t (:trace s)] (if (seq? t) t ()))))
                       (= trace :changes) (if (= exec-result s)
                                            exec-result
                                            (assoc exec-result
                                              :trace
                                              (cons exec-top (let [t (:trace s)] (if (seq? t) t ())))))))]
             (when print-steps
               (printf "\nState after %s steps (last step: %s):\n"
                       iteration (if (seq? exec-top) "(...)" exec-top))
               (state-pretty-print s))
             (when save-state-sequence
               (swap! saved-state-sequence #(conj % s)))
             (recur (inc iteration) s time-limit))))))))

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
   (swap! program-executions-count inc)
   (let [s (if @global-top-level-push-code (push-item code :code state) state)]
     (let [s (if (:calculate-mod-metrics state)
               (push-item (attach-ids (not-lazy code)  (assign-ids (not-lazy code) (atom -1) 0))  :exec s) ; each instruction gets converted into a map {id: identifier :instr old instruction}
               (push-item (not-lazy code) :exec s))]
       (when print-steps
         (printf "\nState after 0 steps:\n")
         (state-pretty-print s))
       (when save-state-sequence
         (reset! saved-state-sequence [s]))
       (let [s (eval-push s print-steps trace save-state-sequence)]
         (if @global-top-level-pop-code
           (pop-item :code s)
           s))))))

