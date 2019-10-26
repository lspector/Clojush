(ns clojush.bench.interpreter-bench
  "Bnchmarks for the interpreter. Runs the interpreter through
   programs, that have been generated randomly during runs."
  (:require [libra.bench :refer :all]
            [libra.criterium :as c]

            [clojush.core :refer [-main]]
            [clojush.interpreter :refer [eval-push]]
            [clojush.bench.core-bench :refer [configurations]]))

(defn grab-call-inputs
  "Returns the first `n` inputs to the `func-var`, from Running
   the cli args.

  Does this by wrapping `func-var` function to save each call, and
  raising an exception after we have enough calls done, which is then caught."
  [func-var n cli-args]
  (let [inputs-outputs-atom (atom (list))
        func-original (var-get func-var)
        done-fn #(>= (count %) n)
        func-log (fn [& input]
                     (let [ret (apply func-original input)
                           inputs-outputs (swap! inputs-outputs-atom conj {:input input :output ret})]
                       (when (done-fn inputs-outputs)
                         (throw (Exception. "")))
                       ret))]

    (with-redefs-fn {func-var func-log}
      (fn []
        (try
          (with-out-str (apply -main cli-args))
          (catch Exception e
            (when-not (done-fn @inputs-outputs-atom)
              (throw e))))))
    @inputs-outputs-atom))

(defmacro defbench-grab-then-benchmark [func-var n-inputs args]
  `(defbench ~(symbol (str (:name (meta (eval func-var))) "-on-" n-inputs "-from-" (first (eval args))))
    (println "Grabbing inputs...")
    (let [total-args# (concat ~args [":use-single-thread" "true"])
          inputs-outputs# (grab-call-inputs ~func-var ~n-inputs total-args#)
          inputs# (map :input inputs-outputs#)
          func# (var-get ~func-var)]
      (println "Running benchmark...")
      (is (c/quick-bench
            (doseq [input# inputs#]
              (apply func# input#)))))))

(defbench-grab-then-benchmark #'eval-push 1000 (:oct-12 configurations))
(defbench-grab-then-benchmark #'eval-push 10000 (:oct-12 configurations))
(defbench-grab-then-benchmark #'eval-push 10000 (:nth-prime configurations))
