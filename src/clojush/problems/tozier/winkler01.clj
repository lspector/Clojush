;; winkler01.clj
;; Bill Tozier, bill@vagueinnovation.com
;;
;; This is code for running my variant on Winkler's Zeros-and-Ones puzzle:
;; For any positive integer input, return a strictly positive integer which
;;   when multiplied by the input produces a result which contains only the
;;   digits 0 and 1 (in base 10 notation)
;;
;; Input and output are given as single integers using the integer stack.

(ns clojush.problems.tozier.winkler01
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random]
        ))

; Create the error function
(defn count-digits [num] (count (str num)))
 
(defn proportion-not-01
    "Returns the proportion of digits in the argument integer which are not 0 or 1"
    [num] 
      (let [counts (frequencies (re-seq #"\d" (str num)))]
      (- 1 
         (/ (+ (get counts "0" 0) 
               (get counts "1" 0)) 
            (count-digits num)))))

(defn winkler-error-function
  "Returns the error function for Tozier's 01 problem."
  [number-test-cases]
  (fn [program]
    (doall
      (for [input (range 1 number-test-cases)]
        (let [final-state (run-push program
          (push-item input :input
            (push-item input :integer
              (make-push-state))))
             result-output (top-item :integer final-state)]
          (if (and (number? result-output) (pos? result-output))
            (proportion-not-01 (* input result-output))
            1000))))))


; Define the argmap
(def argmap
  {:error-function (winkler-error-function 80)
   :max-points 1000
   :max-genome-size-in-initial-program 500
   :evalpush-limit 800
   :population-size 1000
   :max-generations 1000
   :parent-selection :lexicase
   :final-report-simplifications 1000
   })
