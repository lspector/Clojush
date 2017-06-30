;; mackey_glass_int.clj
;;
;; Mackey-Glass chaotic time-series. Langdon and Banzhaf 2005. 
;; Natural Computing, Volume 7, Number 4, 589-613, DOI: 10.1007/s11047-007-9038-8 
;;
;; Exact form of the Mackey-Glass equations is available at:
;;   http://www.scholarpedia.org/article/Mackey-Glass_equation
;;
;; Note: this is the INT version.
;;
;; Kyle Harrington, kyleh@cs.brandeis.edu, 2011

(ns clojush.problems.integer-regression.mackey-glass-int
  (:use [clojush.pushgp.pushgp]
        [clojush.pushstate]
        [clojush.interpreter]
        [clojush.random]
        [local-file])
  (:require [clojure.math.numeric-tower :as math]
            [clojure.string :as s]))

(defn read-data []
  (let [f (slurp (file* "src/clojush/problems/integer_regression/data/mg_int_128.dat"))
        lines (doall (map #(filter (partial not= "")
                                   (s/split % #" "))
                          (s/split-lines f)))]
    (map #(map read-string %) lines)))

(defn data-with-historical
  "Take the data and make a sequence:
              t-128 t-64 t-32 t-16 t-8 t-4 t-2 t-1
   where time points before the start of the series are set to 0."
  [data]
  (doall 
    (for [t (range (count data))]
      (map #(if (< t %) 0 (second (nth data (- t %))))
           '(128 64 32 16 8 4 2 1 0)))))

(def data (data-with-historical (read-data)))

(define-registered x1 
  (fn [state] (push-item (stack-ref :auxiliary 7 state) :integer state)))

(define-registered x2 
  (fn [state] (push-item (stack-ref :auxiliary 6 state) :integer state)))

(define-registered x4 
  (fn [state] (push-item (stack-ref :auxiliary 5 state) :integer state)))

(define-registered x8 
  (fn [state] (push-item (stack-ref :auxiliary 4 state) :integer state)))

(define-registered x16 
  (fn [state] (push-item (stack-ref :auxiliary 3 state) :integer state)))

(define-registered x32
  (fn [state] (push-item (stack-ref :auxiliary 2 state) :integer state)))

(define-registered x64 
  (fn [state] (push-item (stack-ref :auxiliary 1 state) :integer state)))

(define-registered x128
  (fn [state] (push-item (stack-ref :auxiliary 0 state) :integer state)))

(defn error-function
  [num-samples individual]
  (assoc individual
         :errors
         (doall
          (for [row (take num-samples (shuffle data))]
            (let [state (run-push (:program individual) 
                                  (assoc (make-push-state)
                                         :auxiliary
                                         (butlast row)))
                  top-integer (top-item :integer state)]
              (if (number? top-integer)
                (math/expt (- top-integer (last row)) 2)
                1000))))))

(def atom-generators
  (list 
    (fn [] (lrand-int 128))
    'x1 'x2 'x4 'x8 'x16 'x32 'x64 'x128
    'integer_div
    'integer_mult
    'integer_add
    'integer_sub
    ))

(defn problem-specific-report 
  [best population generation sampled-error-function report-simplifications] 
  (let [errors (:errors (error-function (count data) best))
        total-error (apply + errors)]
    #_(println "Best's errors on full data set:" errors)
    (println "Best's total-error on full data set:" total-error)))

(def argmap
  {:error-function (partial error-function 200);; Use 200 random samples
   :atom-generators atom-generators
   :report-simplifications 0
   :max-points 1000
   :max-genome-size-in-initial-program 500
   :evalpush-limit 500
   :population-size 1000
   :epigenetic-markers []
   :genetic-operator-probabilities {:alternation 0.8
                                    :uniform-mutation 0.1
                                    :reproduction 0.1}
   :alignment-deviation 10
   :parent-selection :tournament
   :tournament-size 7
   :max-generations 1000
   :reuse-errors false ;; If a sample set is used, then error reuse must be disabled
   :problem-specific-report problem-specific-report
   })
