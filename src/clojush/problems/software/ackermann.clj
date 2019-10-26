;; ackermann.clj
;; Anil Saini, aks@cs.umass.edu, 2017
;; 
;; Tries to come up with Ackermann Function (https://en.wikipedia.org/wiki/Ackermann_function) 
;; using the first 22 elements of the sequence. Ackermann function, is one of the simplest examples 
;; of a total computable function that is not primitive recursive. 
;;
(ns clojush.problems.software.ackermann
  (:use [clojush.pushgp.pushgp]
        [clojush.pushstate]
        [clojush.interpreter]
        [clojush.random]
        clojush.instructions.tag
        [clojure.math.numeric-tower]))

;; 
(defn ackermann
  "Returns the value of Ackerman function on [m n]"
  [m n]
  (if (= m 0)
    (+ n 1)
    (if (= n 0)
    (ackermann (- m 1) 1)
           (ackermann (dec m) (ackermann m (dec n))))))

(def ackermann-atom-generators
  (concat (list
            (fn [] (lrand-nth (list true false))) ;Boolean ERC
            ;;; end ERCs
            (tag-instruction-erc [:integer :boolean :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            'in2
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :exec])))

;; Currently using only the first 22 elements of the sequence because the rest either take too much time to compute, 
;; or are too big to be represented in a normal machine.
(def argmap
 {:error-function (fn [individual]
                    (assoc individual
                           :errors
                           (doall
                            (for [[inp1 inp2] (take 22 (reverse (into '() (for [i (range 5) j (range 5)]
                                                                            [i j]))))]
                              (let [final-state (run-push (:program individual)
                                                          (->> (make-push-state)
                                                            (push-item inp1 :input)
                                                            (push-item inp2 :input)))                                                              
                                    result (top-item :integer final-state)
                                    desired-output (if (= [inp1 inp2] [4 1])
                                                     65533
                                                     (ackermann inp1 inp2))]
                                (if (number? result)
                                  (abs (- desired-output result))
                                  10000))))))
  :atom-generators ackermann-atom-generators
  :population-size 500
  :max-generations 1000
  :parent-selection :lexicase
  :epigenetic-markers []
  :genetic-operator-probabilities {:alternation 0.5
                                   :uniform-mutation 0.5}
  :uniform-mutation-constant-tweak-rate 0.8
  })
