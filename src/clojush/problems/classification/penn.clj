;; penn.clj
;;
;; Lee Spector, lspector@hampshire.edu, 20170607
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

;; This problem file allows for experimentation with Penn Machine Learning Benchmarks problems,
;; or other other problems with data in the Penn ML format.
;;
;; A random 50/50 split of the data will be created each time this is run, with one half of the
;; split used for training and the other half reserved for testing, which is done only for to
;; report on the best (lowest total training error) individual at the end of each generation. 
;;
;; Although 50% of the data in the file is *available* for use in training, this implementation
;; allows one to specify that only some proportion of that data is actually used. In addition,
;; it allows for the specific subset of training that is used to be resampled each generation.
;;
;; Unlike most Clojush problems, this one contains parameters that cannot be set from the command
;; line. This is because they are required for reading in the data, which currently happens when 
;; this file is loaded, before the command-line parameters are interpreted. This means that to 
;; change these aspects of the problem you must run it from the source code (not just including 
;; Clojush as a dependency in another project) and edit/save this file prior to running the 
;; system. Other parameters can still be set from the command line, overriding the parameters
;; specified in the argmap definition below.
;;
;; Access to the input variables could, in principle, be handled in various ways. What is done
;; here is to provide a boolean input for whether each input variable is each of the values
;; that appear in the data. For example, for some of the target data files the variables in
;; the data are all 0, 1, or 2. In this case, evolving programs will have access to three
;; boolean inputs for each input variable. If a particular variable is 0 then these inputs
;; will be true, false, false, while if it is 1 they will be false, true, false, and if it
;; is 2 they will be false, false, true.
;;
;; Similarly, specification of the program output could, in principle, be handled in various
;; ways. What is done here is to provide string literals for each of the values in the "CLASS"
;; (output) column in the data, and to use the :string stack only for the purpose of accumulating
;; instances of these literals, which act as votes. At the end of program execution, whichever
;; literal occurs most frequently on the :string stack is taken to be the output of the program.
;;
;; Some of the values for standard pushgp parameters specified here, in the argmap definition
;; below, are intended to help lineages weather the disruptions caused by generational resampling
;; of the training data. Specifically, we specify the use of leaky lexicase selection with
;; a high leakage value, and the production of some offspring with very low variation rates.

;; Here are the three data-related parameters that can only be set here, in the source code:

;; The data file, which must be in src/clojush/problems/classification/data/:
(def data-file 
  ;"GAMETES_Epistasis_2-Way_20atts_0.4H_EDM-1_1.txt"
  ;"a_5000s_2000her_0.1__maf_0.2_EDM-1_01.txt"
  ;"xor_2_a_20s_1600_EDM-1_01.txt"
  "xor_3_a_20s_1600_EDM-1_01.txt"
  ;"xor_4_a_20s_1600_EDM-1_01.txt"
  )

;; The proportion of the training data that will be used to evaluate individuals each generation:
(def training-proportion 0.2)

;; A flag indicating whether the subset of the training data used for evaluation should be 
;; resampled each generation:
(def resampling true)

;; Here we define functions to read the data and to split it into training and testing sets.
;; We also extract a subset of the full training set to use for evaluation if training-proportion
;; is less than 1, and we store this in an atom so that we can update the training data
;; if resampling is true.

(defn read-data []
  "Reads data into a sequence of sequences."
  (let [f (slurp* (str "src/clojush/problems/classification/data/" data-file))
        lines (csv/parse-csv f :delimiter \tab)]
    (println "Total number of data lines:" (count lines))
    (mapv #(mapv read-string %) lines)))

(defn define-fitness-cases
  "Returns a map with two keys: train and test. Train maps to a
   random 50% of the fitness cases and test maps to the remainder.
   These sets are different each time this is called."
  []
  (let [raw-data (read-data)
        target-column (.indexOf (mapv clojure.string/upper-case (mapv name (first raw-data)))
                                "CLASS")
        vocabulary (sort (distinct (flatten (rest raw-data))))
        inputs (fn [row] 
                 (let [raw-inputs (concat (take target-column row) 
                                          (drop (inc target-column) row))]
                   (vec (flatten (for [i raw-inputs]
                                   (map #(= i %) vocabulary)))))) ;; inputs are boolean
        target (fn [row] (str (nth row target-column))) ;; target classes are strings
        fitness-cases-shuffled (lshuffle (mapv (fn [row]
                                                 {:inputs (inputs row)
                                                  :target (target row)})
                                               (rest raw-data)))
        train-num (int (* 0.5 (count fitness-cases-shuffled)))
        all-training-cases (subvec fitness-cases-shuffled 0 train-num)
        all-testing-cases (subvec fitness-cases-shuffled train-num)]
    {:all-train all-training-cases
     :train (vec (take (int (* training-proportion train-num)) (lshuffle all-training-cases)))
     :test all-testing-cases}))

(def penn-fitness-cases (atom (define-fitness-cases)))

;; The error function runs the program on the specified subset of the data, returning 
;; errors of 0 whenever the resulting string stack has the correct answer as its most 
;; frequent value, and 1 otherwise.

(defn penn-error-function
  "Error function for the penn problem."
  [fitness-set individual]
  (assoc individual
         :errors
         (doall
          (for [fitness-case (get @penn-fitness-cases fitness-set)]
            (let [inputs (:inputs fitness-case)
                  target (:target fitness-case)
                  push-state (run-push (:program individual) (assoc (make-push-state) :input inputs))
                  [most second-most] (take 2 (reverse (sort-by val (frequencies (:string push-state)))))
                  answer (if (or (not most)
                                 (= (second most) (second second-most)))
                           nil
                           (first most))]
              (if (= answer target) 0 1))))))
            
(defn rmse
  "Returns the root of the mean square error for use in error reporting."
  [errors]
  (sqrt (/ (apply + (map #(* % %) errors))
           (count errors))))            

(defn penn-report
  "Customized generational report for the penn problem, which also resamples (and prints) 
  the training cases if appropriate."
  [best population generation error-function report-simplifications]
  (let [best-program (not-lazy (:program best))
        best-test-errors (:errors (penn-error-function :test {:program best-program}))]
    (printf ";; -*- Penn problem report generation %s" generation)(flush)
    (printf  "\nTest mean: %.4f"
            (float (/ (apply + best-test-errors)
                      (count best-test-errors))))(flush)
    (printf "\nTest RMSE: %.4f" (float (rmse best-test-errors)))(flush)
    (printf "\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n\n")(flush)
    (when (and resampling (< training-proportion 1))
      (println "Resampling training cases...")
      (swap! penn-fitness-cases 
             #(assoc % :train (vec (take (count (:train %))
                                         (lshuffle (:all-train %))))))
      (println "New training cases:")
      (println (:train @penn-fitness-cases)))))

(defn penn-initial-report
  "Initial report function for the penn problem, which prints the training and testing cases."
  [argmap]
  (println "Train and test cases:")
  (println @penn-fitness-cases)
  (println ";;******************************"))

(defn cycle-to-longest
  "A utility for producing a collection with equal representation from multiple sequences.
  The longest sequence will be included in its entirety, and shorter ones will be cycled
  as necessary to produce the same number of elements."
  [& sequences]
  (let [max-count (apply max (map count sequences))]
    (vec (apply concat (map #(take max-count (cycle %)) sequences)))))

(def penn-atom-generators
  (cycle-to-longest 
    ;; input instructions 
    (for [n (map inc (range (count (:inputs (first (:train @penn-fitness-cases))))))]
      (symbol (str "in" n)))
    ;; output class strings
    (distinct (mapv :target (:all-train @penn-fitness-cases)))
    ;; other instructions and ephemeral random constants
    (concat (registered-for-stacks [:exec :integer :boolean :parentheses])
            ;; for strings, which are used only for output class literals, just stack instructions
            '[string_pop string_dup string_dup_times string_dup_items  string_swap string_rot 
              string_flush string_eq string_stackdepth string_yank string_yankdup string_shove 
              string_empty]
            [(tag-instruction-erc [:exec :integer :boolean :string] 1000)
             (tagged-instruction-erc 1000)
             (fn [] (lrand-int 1000))])
    ;; more of anything that uses booleans
    (map first 
         (filter (fn [[instr instr-fn]]
                   (some #{boolean} (:stack-types (meta instr-fn))))
                 @instruction-table))))

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
   :parent-selection :leaky-lexicase ;:lexicase
   :lexicase-leakage 0.5
   :genetic-operator-probabilities {:uniform-addition-and-deletion 0.5
                                    :alternation 0.5}
   :uniform-addition-and-deletion-rate [0.001 0.01 0.1]
   :alternation-rate [0.001 0.01 0.1]
   :alignment-deviation [0 1 10 100]
   :problem-specific-report penn-report
   :problem-specific-initial-report penn-initial-report
   ;:print-behavioral-diversity true ;; requires maintaining @population-behaviors 
   :report-simplifications 0
   :final-report-simplifications 1000
   ;; === parameters for experimenting with age-mediated parent selection (AMPS)
   ;:age-mediated-parent-selection [0.05 0.5]
   ;:age-combining-function :max
   ;:age-combining-function :proportionate 
   ;:age-combining-function :first-reuse 
   ;; === parameters for experimenting with autoconstruction
   ;:autoconstructive true
   ;:autoconstructive-genome-instructions :uniform
   ;:autoconstructive-diversification-test :three-gens-size-and-instruction
   ;:autoconstructive-si-children 2
   ;:autoconstructive-decay 0.1
   })

