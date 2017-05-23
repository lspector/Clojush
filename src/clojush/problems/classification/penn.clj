;; penn.clj
;;
;; Lee Spector, lspector@hampshire.edu, 20170522
;;
;; Adapted from bioavailibility.clj.

(ns clojush.problems.classification.penn
  (:use [clojush.pushgp.pushgp]
        [clojush random util pushstate interpreter]
        clojush.instructions.tag
        [local-file]
        [clojure.math.numeric-tower])
  (:require [clojure.string :as string]
            [clojure-csv.core :as csv]))

(defn rmse
  "Returns the root of the mean square error for use in error reporting."
  [errors]
  (sqrt (/ (apply + (map #(* % %)
                         errors))
           (count errors))))

(defn read-data []
  "Reads data into a sequence of sequences."
  (let [f (slurp* (str "src/clojush/problems/classification/data/"
                       "GAMETES_Epistasis_2-Way_20atts_0.4H_EDM-1_1.txt")) ;; todo: allow command-line arg
        lines (csv/parse-csv f :delimiter \tab)]
    (mapv #(mapv read-string %) lines)))

(def training-proportion 0.1) ;; note should be 0.5 to accord with prior GAMETES experiments

(defn define-fitness-cases
  "Returns a map with two keys: train and test. Train maps to a
   random 50% of the fitness cases and test maps to the remainder.
   These sets are different each time this is called."
  []
  (let [raw-data (read-data)
        target-column (.indexOf (first raw-data) 'class)
        inputs (fn [row] 
                 (concat (take target-column row) 
                         (drop (inc target-column) row)))
        target (fn [row] (str (nth row target-column))) ;; target classes are strings
        fitness-cases-shuffled (shuffle (map (fn [row]
                                               {:inputs (inputs row)
                                                :target (target row)})
                                             (rest raw-data)))
        train-num (int (* penn-training-proportion (count fitness-cases-shuffled)))]
    {:train (subvec fitness-cases-shuffled 0 train-num)
     :test (subvec fitness-cases-shuffled train-num)}))

;; Define the fitness cases once per run, so that train and test
;; subsets stay the same throughout a run.
(def penn-fitness-cases (define-fitness-cases))

(defn penn-error-function
  "Error function for the penn problem."
  [fitness-set program]
  (doall
    (for [fitness-case (get penn-fitness-cases fitness-set)]
      (let [inputs (:inputs fitness-case)
            target (:target fitness-case)
            state (run-push program
                            (loop [to-push (reverse inputs)
                                   with-inputs (make-push-state)]
                              (if (empty? to-push)
                                with-inputs
                                (recur (rest to-push)
                                       (push-item (first to-push)
                                                  :input
                                                  with-inputs)))))
            top-string (top-item :string state)]
        (if (string? top-string)
          (if (= top-string target) 0 1)
          10000)))))

(defn penn-report
  "Customized generational report."
  [best population generation error-function report-simplifications]
  (let [best-program (not-lazy (:program best))
        best-test-errors (penn-error-function :test best-program)]
    (printf ";; -*- Penn problem report generation %s" generation)(flush)
    (printf  "\nTest mean: %.4f"
            (float (/ (apply + best-test-errors)
                      (count best-test-errors))))(flush)
    (printf "\nTest RMSE: %.4f" (float (rmse best-test-errors)))(flush)
    (printf "\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n\n")(flush)
    ))

(defn penn-initial-report
  [argmap]
  (println "Train and test cases:")
  (println penn-fitness-cases)
  (println ";;******************************"))

(def penn-atom-generators
  (concat (distinct (mapv :target (:train penn-fitness-cases))) ;; classes, for output
          ;(list (tag-instruction-erc [:exec :integer :boolean :string] 1000)
          ;      (tagged-instruction-erc 1000))
          (for [n (map inc 
                       (range (count (:inputs (first (:train penn-fitness-cases))))))]
            (symbol (str "in" n)))
          (registered-for-stacks [:exec :integer :boolean])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main call

(def argmap
  {:error-function (partial penn-error-function :train)
   :atom-generators penn-atom-generators
   :max-points 3200
   :max-genome-size-in-initial-program 400
   :evalpush-limit 1600
   :population-size 1000
   :max-generations 300
   :parent-selection :lexicase
   :genetic-operator-probabilities {:alternation 0.2
                                    :uniform-mutation 0.2
                                    :uniform-close-mutation 0.1
                                    [:alternation :uniform-mutation] 0.5
                                    }
   :alternation-rate 0.01
   :alignment-deviation 10
   :uniform-mutation-rate 0.01
   :problem-specific-report penn-report
   :problem-specific-initial-report penn-initial-report
   ;:print-behavioral-diversity true ;; requires maintaining @population-behaviors 
   :report-simplifications 0
   :final-report-simplifications 5000
   })


