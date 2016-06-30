 ;; mux_6.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2010-2012
;;
;; This is code for the 6-bit multiplexer problem.

(ns clojush.problems.boolean.mux-6
  (:use [clojush.pushgp.pushgp]
        [clojush.pushstate]
        [clojush.interpreter]
        [clojure.math.numeric-tower]))

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

(define-registered a0 (with-meta (a 0) {:stack-types [:boolean]}))
(define-registered a1 (with-meta (a 1) {:stack-types [:boolean]}))

(defn d
  [i]
  (fn [state] 
    (push-item (nth (second (:auxiliary state)) i)
               :boolean state)))

(define-registered d0 (with-meta (d 0) {:stack-types [:boolean]}))
(define-registered d1 (with-meta (d 1) {:stack-types [:boolean]}))
(define-registered d2 (with-meta (d 2) {:stack-types [:boolean]}))
(define-registered d3 (with-meta (d 3) {:stack-types [:boolean]}))

(def argmap
  {:error-function (fn [program]
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
   :max-points 800
   :max-genome-size-in-initial-program 200
   :genetic-operator-probabilities {:uniform-close-mutation 0.1
                                    :alternation 0.45
                                    :uniform-mutation 0.45}
   :parent-selection :tournament
   })
