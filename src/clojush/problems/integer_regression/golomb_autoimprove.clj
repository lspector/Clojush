;; golomb_autoimprove.clj
;; a problem for clojush, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2017

;; The problem is to produce, from integer n >= 2, the length of 
;; the shortest (optimal) Golomb ruler with n marks.
;; See http://oeis.org/A003022

;; In this version of the problem, autoconstruction is specified
;; as the default and a factor based on case improvement is built
;; into the error for each case.

(ns clojush.problems.integer-regression.golomb-autoimprove
  (:use [clojush.pushgp.pushgp]
        [clojush pushstate interpreter globals]
        [clojure.math.numeric-tower]))


(def known ;; http://oeis.org/A003022
  (map vector
       (iterate inc 2)
       [1,3,6,11,17,25,34,44,55,72,85,106,127,151,177,
        199,216,246,283,333,356,372,425,480,492,553]))

(def num-train 18)

(def training (take num-train known))

(def testing (drop num-train known))

(defn golomb-error
  [ind cases]
  (let [behaviors (vec (for [in (map first cases)]
                         (->> (make-push-state)
                              (push-item in :input)
                              (run-push (:program ind))
                              (top-item :integer))))
        raw-errors (mapv (fn [behavior case]
                           (if (number? behavior)
                             (abs (- behavior (second case)))
                             100000000N))
                         behaviors
                         cases)
        errors (if (empty? (rest (:history ind)))
                 (mapv #(if (zero? %) 0 (inc %)) raw-errors)
                 (vec (for [case-history (map cons raw-errors (apply map list (:history ind)))]
                        (if (zero? (first case-history))
                          0 ;; solved, improvement doesn't matter
                          (let [improvements (mapv (fn [[newer-error older-error]]
                                                     (let [imp (- older-error newer-error)]
                                                       ;; ignore improvement due to improvement
                                                       (if (> imp 0.75) 
                                                         1.0
                                                         0.0)))
                                                   (partition 2 1 case-history))
                                weights (iterate (partial * 0.5) 0.5)
                                sum (reduce + (mapv * improvements weights))]
                            (if (<= sum 0)
                              1.0E10
                              (+ (first case-history) (- 1.0 sum))))))))]
    (assoc ind
      :behaviors behaviors
      :errors errors)))

(defn golomb-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-with-test (golomb-error best testing)
        best-test-errors (:errors best-with-test)
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Golomb report - generation %s\n" generation)(flush)
    (println "Test total error for best:" best-total-test-error)
    (println (format "Test mean error for best: %.5f" 
                     (double (/ best-total-test-error 
                                (count best-test-errors)))))
    (println ";;******************************")
    best))


(def argmap
  {:error-function (fn [ind] (golomb-error ind training))
   :atom-generators (into [0 1 0.0 1.0 Math/PI Math/E 'in1]
                          (registered-for-stacks
                            [:integer :float :boolean :exec]))
   :parent-selection :lexicase
   :report-simplifications 0
   :problem-specific-report golomb-report
   :autoconstructive true 
   :autoconstructive-genome-instructions :all 
   :autoconstructive-diversification-test :not-a-clone 
   :max-points 800 
   :max-genome-size-in-initial-program 100 
   :evalpush-limit 800 
   :max-generations 10000 
   :print-history true})

