 ;; mux_6.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2010-2012
;;
;; This is code for the 6-bit multiplexer problem.

(ns clojush.examples.mux-6
  (:use [clojush.pushgp.pushgp]
        [clojush.pushstate]
        [clojush.interpreter]
        [clojure.math.numeric-tower]))

(in-ns 'clojush.pushgp.genetic-operators)

#_(defn mutate 
  "Returns a mutated version of the given individual."
  [ind mutation-max-points max-points atom-generators]
  (let [new-program (case (lrand-int 5)
                      0 (insert-code-at-point (:program ind) 
                                              (select-node-index (:program ind))
                                              (random-code mutation-max-points atom-generators))
                      1 (list 'if-dirty (:program ind) (random-code mutation-max-points atom-generators))
                      2 (list 'if-dirty (random-code mutation-max-points atom-generators) (:program ind))
                      3 (list 'if-obstacle (:program ind) (random-code mutation-max-points atom-generators))
                      4 (list 'if-obstacle (random-code mutation-max-points atom-generators) (:program ind)))]
    (if (> (count-points new-program) max-points)
      ind
      (make-individual :program new-program :history (:history ind)
                       :ancestors (if maintain-ancestors
                                    (cons (:program ind) (:ancestors ind))
                                    (:ancestors ind))))))

(defn crossover 
  "Returns a copy of parent1 with a random subprogram replaced with a random 
   subprogram of parent2."
  [parent1 parent2 max-points]
  (let [new-program (case (lrand-int 2)
                      0 (insert-code-at-point 
                          (:program parent1) 
                          (select-node-index (:program parent1))
                          (code-at-point (:program parent2)
                                         (select-node-index (:program parent2))))
                      1 (list (random-code 10 @global-atom-generators) 'exec_if (:program parent1) (:program parent2)))]
    (if (> (count-points new-program) max-points)
      parent1
      (make-individual :program new-program :history (:history parent1)
                       :ancestors (if maintain-ancestors
                                    (cons (:program parent1) (:ancestors parent1))
                                    (:ancestors parent1))))))

(in-ns 'clojush.examples.mux-6)


;; We store address bits in a vector on top of the auxiliary stack
;; and data bits in a vector under the address bits vector.

(defn int->bits-unmemoized
  [i num-bits]
  (let [conversion (Integer/toString i 2)]
    (concat (repeat (- num-bits (count conversion)) false)
            (map #(= \1 %) conversion))))

(def int->bits (memoize int->bits-unmemoized))

(defn bits->int-unmemoized
  [bits]
  (loop [remaining bits total 0]
    (if (empty? remaining)
      total
      (recur (drop 1 remaining)
             (+ total (* (if (first remaining) 1 0) (expt 2 (dec (count remaining)))))))))

(def bits->int (memoize bits->int-unmemoized))

(defn a
  [i]
  (fn [state] 
    (push-item (nth (first (:auxiliary state)) i)
               :boolean state)))

(define-registered a0 (a 0))
(define-registered a1 (a 1))

(defn d
  [i]
  (fn [state] 
    (push-item (nth (second (:auxiliary state)) i)
               :boolean state)))

(define-registered d0 (d 0))
(define-registered d1 (d 1))
(define-registered d2 (d 2))
(define-registered d3 (d 3))

(pushgp 
  :error-function (fn [program]
                    (doall
                      (for [i (range 64)]
                        (let [bits (int->bits i 6)
                              address-bits (vec (take 2 bits))
                              data-bits (vec (drop 2 bits))
                              state (run-push program 
                                              (push-item address-bits :auxiliary 
                                                         (push-item data-bits :auxiliary 
                                                                    (make-push-state))))
                              top-bool (top-item :boolean state)]
                          (if (= top-bool :no-stack-item)
                            1000000
                            (if (= top-bool (nth data-bits (bits->int address-bits)))
                              0
                              1))))))
  :atom-generators '(exec_if boolean_and boolean_or boolean_not
                               a0 a1
                               d0 d1 d2 d3
                               ;boolean_dup boolean_swap boolean_pop boolean_rot
                               )
  :max-points 200
  :max-points-in-initial-program 20
  :use-lexicase-selection true
  :mutation-probability 0.1
  :crossover-probability 0.5
  :simplification-probability 0.4
  )
