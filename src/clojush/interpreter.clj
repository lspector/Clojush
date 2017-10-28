(ns clojush.interpreter
  (:use [clojush pushstate globals util]
        [clojush.instructions tag input-output]
        [clojush.experimental.tagged-code-macros]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; push interpreter

(defprotocol Executable
  (execute [_ state])
  (execute-in-vector [_ state v]))


(defn handle-undefined [instruction]
  (throw (Exception. (str "Undefined instruction: " (pr-str instruction) (type instruction)))))

(extend-protocol Executable
  nil
  (execute-instruction [_ state] state)

  clojure.lang.Symbol
  (execute [instruction state]
    (if-let [instruction-fn (@instruction-table instruction)]
      (instruction-fn state)
      (cond
        (re-seq #"in\d+" (name instruction))
        (handle-input-instruction instruction state)
        (tag-instruction? instruction)
        (handle-tag-instruction instruction state)
        (tagged-code-macro? instruction)
        (handle-tag-code-macro instruction state)
        :else (handle-undefined instruction))))

  BigInteger
  (execute [instruction state] (push-item instruction :integer state))
  (execute-in-vector [_ state v] (push-item v :vector_integer state))

  Short
  (execute [instruction state] (push-item instruction :integer state))
  (execute-in-vector [_ state v] (push-item v :vector_integer state))

  clojure.lang.BigInt
  (execute [instruction state] (push-item instruction :integer state))
  (execute-in-vector [_ state v] (push-item v :vector_integer state))

  java.lang.Integer
  (execute [instruction state] (push-item instruction :integer state))
  (execute-in-vector [_ state v] (push-item v :vector_integer state))

  java.lang.Long
  (execute [instruction state] (push-item instruction :integer state))
  (execute-in-vector [_ state v] (push-item v :vector_integer state))

  java.lang.Float
  (execute [instruction state] (push-item instruction :float state))
  (execute-in-vector [_ state v] (push-item v :vector_float state))

  java.lang.Double
  (execute [instruction state] (push-item instruction :float state))
  (execute-in-vector [_ state v] (push-item v :vector_float state))

  java.lang.Character
  (execute [instruction state] (push-item instruction :char state))
  (execute-in-vector [_ state v] (push-item v :vector_char state))

  java.lang.String
  (execute [instruction state] (push-item instruction :string state))

  java.lang.Boolean
  (execute [instruction state] (push-item instruction :boolean state))
  (execute-in-vector [_ state v] (push-item v :vector_boolean state))

  clojure.lang.PersistentVector
  (execute [instruction state]
    (if (= [] instruction)
      (->> state
        (push-item [] :vector_boolean)
        (push-item [] :vector_string)
        (push-item [] :vector_float)
        (push-item [] :vector_integer))
      (execute-in-vector (nth instruction 0) state instruction)))

  java.lang.Object
  (execute [instruction _]
     (handle-undefined instruction))
  (execute-in-vector [_ _ instruction]
     (handle-undefined instruction)))

(defn execute-instruction
  "Executes a single Push instruction."
  [instruction state]
  ;; for debugging only, e.g. for stress-test
  ;(def debug-recent-instructions (cons instruction debug-recent-instructions))
  ;(def debug-recent-state state)
  (swap! point-evaluations-count inc)
  (execute instruction state))

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
            (let [s (if (seq? exec-top)
                      (assoc s :exec (concat exec-top (:exec s)))
                      (let [execution-result (execute-instruction exec-top s)]
                        (cond
                          (= trace false) execution-result
                          (= trace true) (assoc execution-result
                                                :trace
                                                (cons exec-top (let [t (:trace s)] (if (seq? t) t ()))))
                          (= trace :changes) (if (= execution-result s)
                                               execution-result
                                               (assoc execution-result
                                                      :trace
                                                      (cons exec-top (let [t (:trace s)] (if (seq? t) t ()))))))))]
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
    (let [s (if @global-top-level-push-code (push-item code :code state) state)]
      (let [s (push-item (not-lazy code) :exec s)]
        (when print-steps
          (printf "\nState after 0 steps:\n")
          (state-pretty-print s))
        (when save-state-sequence
          (reset! saved-state-sequence [s]))
        (let [s (eval-push s print-steps trace save-state-sequence)]
          (if @global-top-level-pop-code
            (pop-item :code s)
            s))))))


