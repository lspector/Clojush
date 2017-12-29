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
