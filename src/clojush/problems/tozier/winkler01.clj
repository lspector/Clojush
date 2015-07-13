;; winkler01.clj
;; Bill Tozier, bill@vagueinnovation.com
;;
;; This is code for running Tozier's variant on Winkler's Zeros-and-Ones puzzle:
;;   For any positive (non-zero) integer input, return a strictly positive integer which
;;   when multiplied by the input value produces a result which contains only the
;;   digits 0 and 1 (in base 10 notation)
;;
;; Input and output are given as integers using the integer stack.

(ns clojush.problems.tozier.winkler01
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random]
        [clojure.math.numeric-tower]
        ))

; Create the error function
(defn count-digits [num] (count (re-seq #"\d" (str num))))


(defn proportion-not-01
    "Returns the proportion of digits in the argument integer which are not 0 or 1"
    [num] 
      (let [counts (frequencies (re-seq #"\d" (str num)))]
      (- 1 
         (/ (+ (get counts "0" 0) 
               (get counts "1" 0)) 
            (count-digits num)))))


(defn kill-trailing-zeroes
  "Returns an integer with all trailing zeroes stripped off"
  [num]
    (read-string (clojure.string/replace (str num) #"(0+)$" ""))
  )


(defn winkler-error-function
  "Returns an error function for Tozier's 01 problem."
  [number-test-cases]
  (fn [program]
    (doall
      (for [input (range 1 number-test-cases)]
        (let [final-state (run-push program
          (push-item input :input
            (push-item input :integer
              (make-push-state))))
             result-output (top-item :integer final-state)]
          (when false (println  ;; change to true to print every result (which is awful)
            (if (and (number? result-output) (pos? result-output))
            (* input result-output)
            "N/A")))
          (if (and (number? result-output) (pos? result-output))
            (proportion-not-01 (kill-trailing-zeroes (* input result-output)))
            100)
          )))))


; Atom generators
(def winkler-atom-generators
  (concat (list
            (fn [] (lrand-int 65536)) ;Integer ERC [0,65536]
            ;;; end ERCs
            'in1
            ;;; end input instructions
            )
            (registered-for-stacks [:integer :float :code :boolean :exec :vector_integer :vector_boolean])))


; Define the argmap
(def argmap
  {:error-function (winkler-error-function 44)
   :atom-generators winkler-atom-generators
   :max-points 1000
   :max-genome-size-in-initial-program 500
   :evalpush-limit 1000
   :population-size 1000
   :max-generations 500
   :parent-selection :lexicase
   :final-report-simplifications 1000
   })
