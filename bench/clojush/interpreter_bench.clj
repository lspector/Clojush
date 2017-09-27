(ns clojush.interpreter-bench
  "Bnchmarks for the interpreter. Runs the interpreter through
   programs, that have been generated randomly during runs."
  (:require [libra.bench :refer :all]
            [libra.criterium :as c]

            [clojush.core :refer [-main]]
            [clojush.interpreter :refer [eval-push]]))

(defn grab-executions
  "Returns the first `n` executions of the interpreter, from Running
   the cli args.

  Does this by wrapping the `run-push` function to save each call, and
  raising an exception after we have enough calls done, which is then caught."
  [n cli-args]
  (let [calls-atom (atom (list))
        eval-push-default eval-push ;; https://www.reddit.com/r/Clojure/comments/5esre1/how_to_call_the_original_function_in_withredefs/daf401z/
        done-fn #(>= (count %) n)
        eval-push-log (fn [state & args]
                        (let [ret (apply eval-push-default state args)
                              calls (swap! calls-atom conj {:input state :output ret})]
                          (when (done-fn calls)
                            (throw (Exception. "")))
                          ret))]

    (with-redefs [eval-push eval-push-log]
      (try
        (with-out-str (apply -main cli-args))
        (catch Exception e
          (when-not (done-fn @calls-atom)
            (throw e))))
      @calls-atom)))

(defmacro defbench-eval-push [number-programs problem]
  `(defbench ~(symbol (str "eval-push-on-" number-programs "-from-" problem))
    (println "Grabbing executions...")
    (let [args# [~problem ":use-single-thread" "true"]
          executions# (grab-executions ~number-programs args#)
          inputs# (into [] (map :input executions#))]
      (println "Running benchmark...")
      (is (c/quick-bench
            (doseq [input# inputs#]
              (eval-push input#)))))))

(defbench-eval-push 1000 "clojush.problems.software.replace-space-with-newline")
(defbench-eval-push 1000 "clojush.problems.integer-regression.nth-prime")
(defbench-eval-push 10000 "clojush.problems.integer-regression.nth-prime")
)