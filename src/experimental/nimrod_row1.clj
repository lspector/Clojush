
;; nimrod_row1.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2012

(ns experimental.nimrod_row1
  (:use [clojush] [clojure.math.numeric-tower]))

;;;;;;;;;;;;
;; Integer symbolic regression for row r=1 of the r vs p graph for nimrod
;; (see Henle and Schlatter).

(define-registered in 
  (fn [state] (push-item (stack-ref :auxiliary 0 state) :integer state)))

(define-registered fin 
  (fn [state] (push-item (float (stack-ref :auxiliary 0 state)) :float state)))

(def fitness-cases
  (doall (map vector
              (iterate inc 1)
              [3 5 8 11 13 17 20 22 25 27 30 32 35 38 40 43])))

(pushgp 
  :error-function (fn [program]
                    (doall
                      (for [[input target] fitness-cases]
                        (let [state (run-push program 
                                              (push-item input :auxiliary 
                                                         (push-item input :integer 
                                                                    (make-push-state))))
                              top-int (top-item :integer state)]
                          (if (number? top-int)
                            (Math/abs (- top-int target))
                            1000)))))
  :atom-generators (concat [(fn [] (lrand-int 10))
                            'in
                            'fin
                            ;'integer_div
                            ;'integer_mult
                            ;'integer_add
                            ;'integer_sub
                            ]
                           (registered-for-type :integer :include-randoms false)
                           (registered-for-type :float :include-randoms false)
                           (registered-for-type :exec :include-randoms false)
                           (registered-for-type :boolean :include-randoms false)
                           )
  :use-single-thread true
  :max-points 150
  :mutation-probability 0.4
  :mutation-max-points 20
  :crossover-probability 0.4
  :simplification-probability 0.1
  :tournament-size 7
  :report-simplifications 100
  :final-report-simplifications 1000
  :reproduction-simplifications 10
  :trivial-geography-radius 50
  :decimation-ratio 1
  :decimation-tournament-size 2
  :evalpush-limit 300
  :evalpush-time-limit 0
  :node-selection-method :unbiased
  :node-selection-leaf-probability 0.1
  :node-selection-tournament-size 2
  :pop-when-tagging true
  :gaussian-mutation-probability 0.0
  :gaussian-mutation-per-number-mutation-probability 0.5
  :gaussian-mutation-standard-deviation 0.1
  :reuse-errors true
  :use-historically-assessed-hardness true
  )
