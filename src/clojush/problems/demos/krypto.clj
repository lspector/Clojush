;; krypto.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2010

(ns clojush.problems.demos.krypto
  (:use [clojush.pushgp.pushgp]
        [clojush.pushstate]
        [clojush.interpreter]
        [clojure.math.numeric-tower]))

;; Chooses and then tries to solve (via pushgp) a game of Krypto.
;; See http://en.wikipedia.org/wiki/Krypto_(game)
;; Might produce solutions that rely on truncation in integer division.

(def deck (shuffle (vec (concat ;; deck with right number of each card
                                (take 3 (repeat 1))
                                (take 3 (repeat 2))
                                (take 3 (repeat 3))
                                (take 3 (repeat 4))
                                (take 3 (repeat 5))
                                (take 3 (repeat 6))
                                (take 4 (repeat 7))
                                (take 4 (repeat 8))
                                (take 4 (repeat 9))
                                (take 4 (repeat 10))
                                (take 2 (repeat 11))
                                (take 2 (repeat 12))
                                (take 2 (repeat 13))
                                (take 2 (repeat 14))
                                (take 2 (repeat 15))
                                (take 2 (repeat 16))
                                (take 2 (repeat 17))
                                [18 19 20 21 22 23 24 25]))))

(def objective (first deck))

(def hand (into () (subvec deck 1 6)))

(println "Objective: " objective ", Hand (top of stack listed first): " hand)

(def argmap
  {:error-function (fn [individual]
                     (assoc individual
                            :errors
                            (doall
                             (list
                              (let [state (run-push (:program individual)
                                                    (assoc (make-push-state) :integer hand))
                                    top-int (top-item :integer state)]
                                (if (and (empty? (rest (:integer state))) (number? top-int))
                                  (abs (- top-int objective))
                                  1000))))))
   :atom-generators (list 'integer_div 'integer_mult 'integer_add 'integer_sub)
   :epigenetic-markers []
   :genetic-operator-probabilities {:alternation 0.5
                                    :uniform-mutation 0.5}
   :parent-selection :tournament
   :tournament-size 3
   })
