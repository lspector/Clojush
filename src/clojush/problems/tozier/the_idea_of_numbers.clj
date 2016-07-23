;; the_idea_of_numbers.clj
;; Bill Tozier, bill@vagueinnovation.com
;;
;; This implements a common pedagogic demonstration from classes I
;;   teach in GP. The objective is a simple symbolic regression problem
;;   except there are no numeric ERCs.
;;
;; Input and output are given as integers using the integer stack.

(ns clojush.problems.tozier.the-idea-of-numbers
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util]
        [clojure.math.numeric-tower]
        ))


(defn birthday-polynomial
  "Returns a polynomial y = YYYY + MM * x * DD * x * x"
  [x year month day]
    (+ year (* month x) (* day x x))
  )


(defn missing-numbers-error-function
  "Returns the absolute error."
  [number-test-cases]
  (fn [program]
    (doall
      (for [input (range 0 number-test-cases)]
        (let [final-state (run-push program
          (push-item input :input
              (make-push-state)))
             result-output (top-item :integer final-state)]
          (if (and (number? result-output))
            (abs (- result-output (birthday-polynomial input 1964 9 11)))  ;; edit this so it's your birthday
            1000000000)
          )))))

; Atom generators
(def missing-numbers-atom-generators
  (cons 'in1
        (registered-for-stacks [:integer :code :boolean :exec :char :string :float])))



; Define the argmap
(def argmap
  {:error-function (missing-numbers-error-function 20)
   :atom-generators missing-numbers-atom-generators
   :max-points 1000
   :max-genome-size-in-initial-program 300
   :evalpush-limit 1000
   :population-size 1000
   :max-generations 300
   :parent-selection :lexicase 
   :final-report-simplifications 1000
   :genetic-operator-probabilities {
     :alternation 0.5
     :uniform-mutation 0.5}
   })
