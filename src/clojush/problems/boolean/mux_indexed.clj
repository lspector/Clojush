 ;; mux_indexed.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2010-2012
;;
;; This is code for multiplexer problems of various sizes, using integers
;; to index address and data bits (which are Boolean values).

(ns clojush.problems.boolean.mux-indexed
  (:use [clojush.pushgp.pushgp]
        [clojush.pushstate]
        [clojush.interpreter]
        [clojush.random]
        [clojure.math.numeric-tower]))

;; We store address bits in a vector on top of the auxiliary stack
;; and data bits in a vector under the address bits vector.

(def number-of-address-bits 2) ;; for 11-mux use 3
(def number-of-data-bits 4) ;; for 11-mux use 8

(defn valid-address-index
  [n]
  (mod (abs n) number-of-address-bits))

(defn valid-data-index
  [n]
  (mod (abs n) number-of-data-bits))

(define-registered a ;; push an address bit, indexed by an integer
                   (fn [state] 
                     (if (not (empty? (:integer state)))
                       (push-item (nth (first (:auxiliary state))
                                       (valid-address-index (first (:integer state))))
                                  :boolean
                                  (pop-item :integer state))
                       state)))

(define-registered d ;; push a data bit, indexed by an integer
                   (fn [state] 
                     (if (not (empty? (:integer state)))
                       (push-item (nth (second (:auxiliary state))
                                       (valid-data-index (first (:integer state))))
                                  :boolean
                                  (pop-item :integer state))
                       state)))

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
  
(def argmap
  {:error-function (fn [program]
                     (let [total-num-bits (+ number-of-address-bits number-of-data-bits)]
                       (doall
                         (for [i (range (expt 2 total-num-bits))]
                           (let [bits (int->bits i total-num-bits)
                                 address-bits (vec (take number-of-address-bits bits))
                                 data-bits (vec (drop number-of-address-bits bits))
                                 state (run-push program 
                                                 (push-item address-bits :auxiliary 
                                                            (push-item data-bits :auxiliary 
                                                                       (make-push-state))))
                                 top-bool (top-item :boolean state)]
                             (if (= top-bool :no-stack-item)
                               1000000
                               (if (= top-bool (nth data-bits (bits->int address-bits)))
                                 0
                                 1)))))))
   :atom-generators (concat
                      [(fn [] (lrand-int (+ number-of-address-bits number-of-data-bits)))]
                      '(a d exec_if boolean_and boolean_or boolean_not
                          ;boolean_dup boolean_swap boolean_pop boolean_rot
                          ;integer_add integer_sub integer_mult integer_div integer_mod
                          ;integer_dup integer_swap integer_pop integer_rot
                          ))
   :max-points 800
   :max-genome-size-in-initial-program 200
   :genetic-operator-probabilities {:uniform-close-mutation 0.1
                                    :alternation 0.45
                                    :uniform-mutation 0.45}
   :parent-selection :tournament
   })
