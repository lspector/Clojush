;; tagged_regression.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2010

(ns clojush.problems.demos.tagged-regression-c
  (:use [clojush util globals pushstate simplification individual] 
        [clojush.pushgp.pushgp]
        [clojush.pushstate]
        [clojush.random]
        [clojush.interpreter]
        [clojure.math.numeric-tower]
        [clojush.instructions.tag]
        [clojush.instructions.environment])
  (:require [clojure.string :as string]
            [clojure.math.numeric-tower :as math]
            [local-file]))

;;;;;;;;;;;;
;; Integer symbolic regression of x^3 - 2x^2 - x (problem 5 from the 
;; trivial geography chapter) with minimal integer instructions
;; ALSO uses tags, although there is little reason to think they would
;; help on such a simple problem.

;; Before executing the individual programs for the purpose of error calculation, 
;; initialize its tagspace by tagging the whole program with a certain number for tags.
;; Types of values to be tagged include single instructions/literals and list of instructions/literals
;; enclosed within brackets.

(defn merge-tagspaces
  [gts gts-fit lts total-error]
  (let [lt (into (sorted-map) (map #(vector (first %1) (vector (second %1) total-error)) (apply vector @lts)))
        gt (if (empty? @gts)
             lt
             (merge-with vector @gts @gts-fit))
        ]
    (doseq [key (keys lt)]
      (if (get gt key)
        (if (> (second (get gt key)) (second (get lt key)))
          (do
            (swap! gts assoc key (first (get lt key)))
            (swap! gts-fit assoc key (second (get lt key))))
          (do
            (swap! gts assoc key (first (get gt key)))
            (swap! gts-fit assoc key (second (get gt key))))
          )
        (if (> (second (val (closest-association key {:tag gt}))) (second (get lt key)))
          (do
            (swap! gts assoc key (first (get lt key)))
            (swap! gts-fit assoc key (second (get lt key))))
          )
        ))
    ))

;; Implements (x^3+1)^3 + 1 
;;
(def fitness-cases
  (for [input (range -3.5 4.0 0.5)]
    [input
     (let [x-new (+ (* input input input) 1)]
       (+ (* x-new x-new x-new x-new x-new)
          1))]))

(def argmap
  {:error-function (fn error-function
                     ([individual]
                      (error-function individual :train))
                     ([individual data-cases] ;; data-cases should be :train or :test
                      (error-function individual data-cases false))
                     ([individual data-cases print-outputs]
                      (let [                                ;errors-gts-smaller? (atom false)
                            ;local-lineage-tagspace
                            ; (case data-cases
                            ;  :train (atom (:tagspace individual))
                            ;  :simplify (atom (:tagspace individual)) ; during simplification and testing, the tagspace should not be changed.
                            ;  :test (atom (:tagspace individual))
                            ;  (atom (:tagspace individual)))
                            local-global-tagspace (case data-cases
                                                    :train (atom @global-common-tagspace)
                                                    :simplify (atom (:tagspace individual)) ; during simplification and testing, the tagspace should not be changed.
                                                    :test (atom (:tagspace individual))
                                                    (atom @global-common-tagspace))
                            ; errors-lts (doall
                            ; (for [[input target] fitness-cases]
                            ; (let [state (run-push (:program individual)
                            ; (push-item input :input
                            ; (push-item input :float
                            ;           (assoc (make-push-state) :tag @local-lineage-tagspace))))
                            ;top-float       (top-item :float state)
                            ;_ (if (not= data-cases :test)
                            ;  (reset! local-lineage-tagspace (get state :tag)))]
                            ; (if (number? top-float)
                            ;  (abs (- top-float target))
                            ;  1000.0))))
                            errors-gts (doall
                                         (for [[input target] fitness-cases]
                                           (let [state (run-push (:program individual)
                                                                 (push-item input :input
                                                                            (push-item input :float
                                                                                       (assoc (make-push-state) :tag @local-global-tagspace))))
                                                 top-float (top-item :float state)
                                                 _ (if (not= data-cases :test)
                                                     (reset! local-global-tagspace (get state :tag)))]
                                             (if (number? top-float)
                                               (abs (- top-float target))
                                               1000.0))))
                            ;_               (reset! errors-gts-smaller? (every? true? (map #(<= %1 %2) errors-gts errors-lts)))
                            _ (if (and (not= data-cases :test) (not= data-cases :simplify))
                                (if (let [                  ;x (vec errors-lts) ;errors of child
                                          y (vec errors-gts)
                                          z (first (:history individual)) ;errors of parent
                                          ]
                                      (if (nil? z)
                                        true
                                        ;    (some? (some true? (map #(< %1 %2) x y))))) ;child is better than mom on at least one test case; can be worse on others
                                        ; (if @errors-gts-smaller? ;min (errors-lts, errors-gts) <= errors of mom ;child is as good or better than mom
                                        ;  (every? true? (map #(<= %1 %2) y z))
                                        ;  (every? true? (map #(<= %1 %2) x z))
                                        true
                                        )
                                      )
                                  (do
                                    (merge-tagspaces global-common-tagspace global-common-tagspace-fitness local-global-tagspace (apply + errors-gts))
                                    )))
                            ]
                        (assoc individual
                          :errors errors-gts :tagspace @local-global-tagspace
                                              ))))
   :atom-generators                    (concat (list (fn [] (lrand 10))
                                                     'in1
                                                     (tag-instruction-erc [:float :integer :exec] 10)
                                                     (tagged-instruction-erc 10)
                                                     (untag-instruction-erc 10)
                                                     (registered-for-type "return_")
                                                     )
                                               (registered-for-stacks [:float :integer :exec]))
   :genetic-operator-probabilities     {:uniform-addition-and-deletion 1}
   :uniform-addition-and-deletion-rate 0.09
   :parent-selection                   :epsilon-lexicase
   :use-single-thread                  true
   :max-generations                    500
   ;:use-lineage-tagspaces              true
   ;:print-history                      true
   :pop-when-tagging                   false
   })
