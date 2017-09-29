(ns clojush.interpreter-bench
  "Bnchmarks for the interpreter. Runs the interpreter through
   programs, that have been generated randomly during runs."
  (:require [libra.bench :refer :all]
            [libra.criterium :as c]

            [clojush.core :refer [-main]]
            [clojush.interpreter :refer [eval-push]]))

(defn grab-call-inputs
  "Returns the first `n` inputs to the `func-var`, from Running
   the cli args.

  Does this by wrapping `func-var` function to save each call, and
  raising an exception after we have enough calls done, which is then caught."
  [func-var n cli-args]
  (let [inputs-atom (atom (list))
        func-original (var-get func-var)
        done-fn #(>= (count %) n)
        func-log (fn [& input]
                        (let [ret (apply func-original input)
                              inputs (swap! inputs-atom conj input)]
                          (when (done-fn inputs)
                            (throw (Exception. "")))
                          ret))]

    (with-redefs-fn {func-var func-log}
      (fn []
        (try
          (with-out-str (apply -main cli-args))
          (catch Exception e
            (when-not (done-fn @inputs-atom)
              (throw e))))))
    @inputs-atom))

(defmacro defbench-grab-then-benchmark [func-var n-inputs args]
  `(defbench ~(symbol (str (:name (meta (eval func-var))) "-on-" n-inputs "-from-" (first args)))
    (println "Grabbing inputs...")
    (let [total-args# [~@args ":use-single-thread" "true"]
          inputs# (grab-call-inputs ~func-var ~n-inputs total-args#)
          func# (var-get ~func-var)]
      (println "Running benchmark...")
      (is (c/quick-bench
            (doseq [input# inputs#]
              (apply func# input#)))))))

(defbench-grab-then-benchmark #'eval-push 1000 ["clojush.problems.software.replace-space-with-newline"])
(defbench-grab-then-benchmark #'eval-push 1000 ["clojush.problems.integer-regression.nth-prime"])
(defbench-grab-then-benchmark #'eval-push 10000 ["clojush.problems.integer-regression.nth-prime"])
