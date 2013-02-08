 ;; mux_11.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2010-2012
;;
;; This is code for the 11-bit multiplexer problem.

(ns clojush.examples.mux-11
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

(define-registered a0 (a 0))
(define-registered a1 (a 1))
(define-registered a2 (a 2))

(defn d
  [i]
  (fn [state] 
    (push-item (nth (second (:auxiliary state)) i)
               :boolean state)))

(define-registered d0 (d 0))
(define-registered d1 (d 1))
(define-registered d2 (d 2))
(define-registered d3 (d 3))
(define-registered d4 (d 4))
(define-registered d5 (d 5))
(define-registered d6 (d 6))
(define-registered d7 (d 7))

(define-push-argmap
  :error-function (fn [program]
                    (doall
                      (for [i (range 2048)]
                        (let [bits (int->bits i 11)
                              address-bits (vec (take 3 bits))
                              data-bits (vec (drop 3 bits))
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
                               a0 a1 a2
                               d0 d1 d2 d3 d4 d5 d6 d7
                               ;boolean_dup boolean_swap boolean_pop boolean_rot
                               )
  :population-size 100
  :max-points 100
  :max-points-in-initial-program 100
  )
