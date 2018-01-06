(ns clojush.bench.helpers
  (:require [clojush.core :refer [-main]]))

(defn call-main [generations serial]
  (with-out-str
    (-main
      "clojush.problems.software.replace-space-with-newline"
      ":autoconstructive" "true"
      ":autoconstructive-genome-instructions" ":all"
      ":autoconstructive-diversification-test" "[:not-a-clone :doesnt-clone]"
      ":report-simplifications" "0"
      ":meta-error-categories" "[:case-stagnation :autoconstruction-blindness]"
      ":print-history" "true"
      ":parent-selection" ":leaky-lexicase"
      ":lexicase-leakage" "0.1"
      ":max-generations" (str generations)
      ":use-single-thread" (str serial))))

(defn grab-call-inputs
  "Returns the first `n` inputs to the `func-symb`, from running
   the cli args.

  Does this by wrapping `func-symb` function to save each call, and
  raising an exception after we have enough calls done, which is then caught."
  [func-symb n]
  (require (symbol (namespace func-symb)))
  (let [inputs-atom (atom (list))
        func-var (eval `(var ~func-symb))
        func-original (var-get func-var)
        done-fn #(>= (count %) n)
        func-log (fn [& input]
                   (when (done-fn (swap! inputs-atom conj input))
                     (throw (Exception. "")))
                   (apply func-original input))]
    (with-redefs-fn {func-var func-log}
      (fn []
        (try
          (call-main 99 true)
          (catch Exception e
            (when-not (done-fn @inputs-atom)
              (throw e))))))
    @inputs-atom))

(defn run-fn [func-symb possible-inputs]
  (require (symbol (namespace func-symb)))
  (let [func-var (eval `(var ~func-symb))
        func (var-get func-var)]
    (apply func (rand-nth possible-inputs))))
