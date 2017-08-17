(ns clojush.test.interpreter-benchmark
  "Tests and benchmarks for the interpreter. Runs the interpreter through
   programs, that have been generated randomly during runs.

   Run `lein run -m clojush.test.interpreter-test/benchmark <n push programs> <problem file> [<args>] `
   to profile the first <n push programs> generated from the problem file."
  (:require [criterium.core]
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


(defn benchmark [number-programs & args]
  (apply println "Getting first" number-programs "push program executions from run" args)
  (let [total-args  (concat args [":use-single-thread" "true"])
        executions (grab-executions (Integer/parseInt number-programs) total-args)
        inputs (into [] (map :input executions))]
    (println "Benchmarking executing them all in succession with criteriom...")
    (criterium.core/with-progress-reporting
      (criterium.core/quick-bench
        (doseq [input inputs]
          (eval-push input))
        :verbose))))
