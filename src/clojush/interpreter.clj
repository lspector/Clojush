(ns clojush.interpreter
  (:use [clojush.pushstate]
        [clojush.globals]
        [clojush.instructions.tag]
        [clojush.experimental.tagged-code-macros]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; push interpreter

(defn recognize-literal
  "If thing is a literal, return its type -- otherwise return false."
  [thing]
  (cond (integer? thing) :integer
        (number? thing) :float
        (string? thing) :string
        (or (= thing true) (= thing false)) :boolean
        true false))

(def debug-recent-instructions ())

(defn execute-instruction
  "Executes a single Push instruction."
  [instruction state]
  ;; for debugging only, e.g. for stress-test
  ;(def debug-recent-instructions (cons instruction debug-recent-instructions))
  ;(def debug-recent-state state)
  (if (= instruction nil) ;; tests for nil and ignores it
    state
    (let [literal-type (recognize-literal instruction)]
      (cond 
        literal-type (push-item instruction literal-type state)
        (tag-instruction? instruction) (handle-tag-instruction instruction state)
        (tagged-code-macro? instruction) (handle-tag-code-macro instruction state)
        :else ((instruction @instruction-table) state)))))

(defn eval-push 
  "Executes the contents of the exec stack, aborting prematurely if execution limits are 
exceeded. The resulting push state will map :termination to :normal if termination was 
normal, or :abnormal otherwise."
  ([state] (eval-push state false false))
  ([state print] (eval-push state print false))
  ([state print trace]
    (loop [iteration 1 s state
           time-limit (if (zero? @global-evalpush-time-limit)
                        0
                        (+' @global-evalpush-time-limit (System/nanoTime)))]
      (if (or (> iteration @global-evalpush-limit)
            (empty? (:exec s))
            (and (not (zero? time-limit))
              (> (System/nanoTime) time-limit)))
        (assoc s :termination (if (empty? (:exec s)) :normal :abnormal))
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
            (when print
              (printf "\nState after %s steps (last step: %s):\n" 
                iteration (if (seq? exec-top) "(...)" exec-top))
              (state-pretty-print s))
            (recur (inc iteration) s time-limit)))))))

(defn run-push 
  "The top level of the push interpreter; calls eval-push between appropriate code/exec 
pushing/popping. The resulting push state will map :termination to :normal if termination was 
normal, or :abnormal otherwise."
  ([code state]
    (run-push code state false false))
  ([code state print]
    (run-push code state print false))
  ([code state print trace]
    (let [s (if top-level-push-code (push-item code :code state) state)]
      (let [s (push-item code :exec s)]
        (when print
          (printf "\nState after 0 steps:\n")
          (state-pretty-print s))
        (let [s (eval-push s print trace)]
          (if top-level-pop-code
            (pop-item :code s)
            s))))))
