;; winkler01.clj
;; Bill Tozier, bill@vagueinnovation.com
;;
;; This is code for running Tozier's variant on Winkler's Zeros-and-Ones puzzle:
;;   For any positive (non-zero) integer input, return a strictly positive integer which
;;   when multiplied by the input value produces a result which contains only the
;;   digits 0 and 1 (in base 10 notation)
;;
;; Input and output are given as integers using the integer stack.

(ns clojush.problems.software.winkler01
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util]
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


(defn kill-trailing-zeros
  "Returns an integer with all trailing zeros stripped off"
  [num]
    (read-string (clojure.string/replace (str num) #"(0+)$" ""))
  )


;; "obvious" first attempt at an error function
(defn winkler-error-function-01
  "Returns the proportion of digits in the product of input * output that are not 0 or 1."
  [number-test-cases]
  (fn [individual]
    (assoc individual
           :errors
           (doall
            (for [input (range 1 number-test-cases)]
              (let [final-state (run-push (:program individual)
                                          (push-item input :input
                                                     (make-push-state)))
                    result-output (top-item :integer final-state)]
                (when false (println  ;; change to true to print every result (which is awful)
                             (if (and (number? result-output) (pos? result-output))
                               (* input result-output)
                               "N/A")))
                (if (and (number? result-output) (pos? result-output))
                  (proportion-not-01 (* input result-output))
                  100)
                ))))))


;; "obvious" second attempt at an error function;
;; accommodation to trivial strategy of multiplying by 10000000...
(defn winkler-error-function-02
  "Returns the proportion of digits in the product of input * output that are not 0 or 1, after trimming trailing zeros."
  [number-test-cases]
  (fn [individual]
    (assoc individual
           :errors
           (doall
            (for [input (range 1 number-test-cases)]
              (let [final-state (run-push (:program individual)
                                          (push-item input :input
                                                     (make-push-state)))
                    result-output (top-item :integer final-state)]
                (when false (println  ;; change to true to print every result (which is awful)
                             (if (and (number? result-output) (pos? result-output))
                               (* input result-output)
                               "N/A")))
                (if (and (number? result-output) (pos? result-output))
                  (proportion-not-01 (kill-trailing-zeros (* input result-output)))
                  100)
                ))))))


;; trying to give it some raw materials it might want to use

(defn prime-factors
  "Return a vector of the prime factors of the argument integer; cadged from http://rosettacode.org/wiki/Prime_decomposition#Clojure"
  ([num]
    (prime-factors num 2 ()))
  ([num k acc]
    (if (= 1 num)      
      acc
      (if (= 0 (rem num k))        
        (recur (quot num k) k (cons k acc))
        (recur num (inc k) acc)))))


(defn prime-factors-as-sorted-vector
  "Return the argument's prime factors as a sorted vector of integers; if the argument is 0, it returns (0); if the argument is negative, it returns the factors of the positive number with -1 added to the list;"
  [num]
  (cond 
    (pos? num) (into [] (sort (prime-factors num)))
    (neg? num) (into [] (cons -1 (sort (prime-factors (abs num)))))
    :else [0]
  ))


; Define new instructions
(define-registered
  integer_factors
  ^{:stack-types [:integer :vector_integer]}
  (fn [state]
    (if (not (empty? (:integer state)))
      (push-item (prime-factors-as-sorted-vector (stack-ref :integer 0 state))
                 :vector_integer
                 (pop-item :integer state))
      state)))


; Atom generators
(def winkler-atom-generators
  (concat (take 100 (repeat 'in1))
          (take 50 (repeat (fn [] (lrand-int 65536)))) ;Integer ERC [0,65536]
          (registered-for-stacks [:integer :code :boolean :exec :vector_integer :char :string :float])))



; Define the argmap
(def argmap
  {:error-function (winkler-error-function-02 44) ;; change the error function to follow along...
   :atom-generators winkler-atom-generators
   :max-points 1000
   :print-csv-logs true
   :csv-columns [:generation :location :parent-uuids :genetic-operators :push-program-size :push-program :total-error :test-case-errors]
   :csv-log-filename "log.csv"
   :max-genome-size-in-initial-program 500
   :evalpush-limit 1000
   :population-size 1000
   :max-generations 1000
   :parent-selection :lexicase 
   :final-report-simplifications 1000
   })
