(ns clojush.core-bench
  (:require [libra.bench :refer :all]

            [clojush.core :refer [-main]]
            [clojush.test.core-test :refer [reset-globals!]]))

(defmacro defbench-main [name args]
  `(defbench ~(symbol (str "main-" name))
    (with-redefs [shutdown-agents (fn [])]
      (is (dur 10 (do (reset-globals!)
                      (with-out-str (apply -main ~args))))))))

(def configurations
  {:jan-13
    ["clojush.problems.software.replace-space-with-newline"
      ":autoconstructive" "true"
      ":autoconstructive-genome-instructions" ":uniform"
      ":autoconstructive-diversification-test" ":size-and-instruction"
      ":autoconstructive-si-children" "2"
      ":autoconstructive-integer-rand-enrichment" "10"
      ":autoconstructive-boolean-rand-enrichment" "10"
      ":max-points" "1600"
      ":final-report-simplifications" "0"
      ":report-simplifications" "0"
      ":max-genome-size-in-initial-program" "400"
      ":evalpush-limit" "1600"
      ":parent-selection" ":leaky-lexicase"
      ":autoconstructive-entropy" "0.1"]})

(defbench-main "autocon-5-gen-jan-13"
  (concat (:jan-13 configurations) [":max-generations" "5"]))

(defbench-main "nth-prime-5-gen"
   ["clojush.problems.integer-regression.nth-prime"
    ":final-report-simplifications" "0"
    ":report-simplifications" "0"
    ":max-generations" "5"])

(defbench cleanup
  (shutdown-agents))