;; rule30_dynamic.clj
;; Lee Spector, lspector@hampshire.edu
;;
;; Given an integer, return a boolean indicating whether the location indexed
;; by the integer in the output of a rule 30 cellular automation is 1.
;; The automaton is started in the state ...00000100000..., and indexing starts
;; at the single "1" in the first row (which has index 0) and then proceeds to 
;; subsequent rows, always covering only the triangle of influence of the 
;; original "1". Graphically, the indices are laid out as:

;; ..........0..........
;; .........123.........
;; ........45678........
;; etc.

;; The problem is dymanic in the sense that a new set of test points is used
;; each generation. A negagive error-threshold is used to prevent the genetic
;; programming system from terminating until the generation limit is reached.

(ns clojush.problems.boolean.rule30-dynamic
  (:use [clojush.ns]))

;; Allow access to all clojush namespaces (except for examples/* and experimental/*)
(use-clojush)

(def number-of-rule30-datapoints 1024)

(def number-of-rule30-datapoints-per-generation 32)

(defn next-row ;; of Rule 30 Cellular Automaton: http://mathworld.wolfram.com/Rule30.html
  [row]
  (let [windows (partition 3 1 (concat [false false] row [false false]))]
    (mapv (fn [window]
            (case window
              [true true true] false
              [true true false] false
              [true false true] false
              [true false false] true
              [false true true] true
              [false true false] true
              [false false true] true
              [false false false] false))
          windows)))

(def rule30-data
  (loop [rows [[true]]
         number-so-far 1]
    (if (>= number-so-far number-of-rule30-datapoints)
      (mapv vector 
            (range number-of-rule30-datapoints)
            (take number-of-rule30-datapoints (flatten rows)))
      (let [row (next-row (last rows))]
        (recur (conj rows row)
               (+ number-so-far (count row)))))))

(def rule30-data-this-generation ;; dynamic, initialized to a random subset
  (atom (vec (take number-of-rule30-datapoints-per-generation 
                   (lshuffle rule30-data)))))

;; print the initial data subset, which will be used in the first generation
(println "rule30-data-this-generation:" @rule30-data-this-generation)

(defn rule30-program-output
  [program input]
  (top-item :boolean 
            (run-push program 
                      (push-item input :input 
                                 (make-push-state)))))

(def argmap
  {:error-function (fn [program]
                     (doall
                       (for [[input output] @rule30-data-this-generation]
                         (if (= (rule30-program-output program input)
                                output)
                           0
                           1))))
   :atom-generators (concat (list (fn [] (- (lrand-int 5) 2))
                                  ;(tag-instruction-erc [:exec :integer :boolean] 1000)
                                  ;(tagged-instruction-erc 1000)
                                  'in1)
                            (registered-for-stacks [:integer :boolean :exec]))
   
   :max-points 1024
   :max-genome-size-in-initial-program 128
   :evalpush-limit 2048
   :genetic-operator-probabilities {:alternation 0.2
                                    :uniform-mutation 0.2
                                    :uniform-close-mutation 0.1
                                    [:alternation :uniform-mutation] 0.5
                                    }
   :alternation-rate 0.01
   :alignment-deviation 10
   :uniform-mutation-rate 0.01
   :parent-selection :lexicase
   :error-threshold -1 ;; never stop
   :problem-specific-report (fn [best population generation error-function report-simplifications]
                              ;; reset the data set to be used
                              ;; (done here because this is called between generations)
                              (reset! rule30-data-this-generation
                                      (vec (take number-of-rule30-datapoints-per-generation 
                                                 (lshuffle rule30-data))))
                                (println "rule30-data-this-generation:" @rule30-data-this-generation))
   :autoconstructive-integer-rand-enrichment -1
   :autoconstructive-boolean-rand-enrichment 7
   })

(defn total-error-on-all-rule30-data ;; for validation
  [program]
  (load-push-argmap argmap)
  (reset-globals)
  (reduce +
          (for [[input output] rule30-data]
            (if (= (rule30-program-output program input)
                   output)
              0
              1))))






