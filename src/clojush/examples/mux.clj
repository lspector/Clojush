 ;; mux.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2010
;;
;; This is code for multiplexer problems of various sizes, using integers
;; to index address and data bits (which are Boolean values).
;;
;; 20110327: Actually there's code both for the "various sizes" version of the 
;; problem (which uses integer-indexed instructions to push address and data bits)
;; and also for fixed size versions, with separate instructions for each address/data
;; bit (see the "int-embedded" instructions below). And there's some other hackage here
;; too because I've been experimenting. So you really have to look through the code
;; in this file before running it.

(ns examples.mux
  (:use [clojush]
        [clojure.math.numeric-tower]))

;;; HACKS to Clojush stuff for experimentation here
;;; Hacks for collecting and printing full ancestor lists
(in-ns 'clojush)
(def maintain-ancestors true)
(def print-ancestors-of-solution true)
(defn crossover 
  "Returns a copy of parent1 with a random subprogram replaced with a random 
   subprogram of parent2."
  [parent1 parent2 max-points]
  (let [new-program (insert-code-at-point 
                      (:program parent1) 
                      (select-node-index (:program parent1))
                      (code-at-point (:program parent2)
                                     (select-node-index (:program parent2))))]
    (if (> (count-points new-program) max-points)
      parent1
      (make-individual :program new-program :history (:history parent1)
                       :ancestors (if maintain-ancestors
                                    (list (list 'XOVER (cons (:program parent1) (:ancestors parent1)) ;;; CHANGED HERE
                                                (cons (:program parent2) (:ancestors parent2)))) ;;; CHANGED HERE
                                    (:ancestors parent1))))))

(in-ns 'examples.mux) ;; end of hacks to clojush.clj

;; We store address bits in a vector on top of the auxiliary stack
;; and data bits in a vector under the address bits vector.

(def number-of-address-bits 2)
(def number-of-data-bits 4)

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

(defn int->bits
  [i num-bits]
  (let [conversion (Integer/toString i 2)]
    (concat (repeat (- num-bits (count conversion)) false)
            (map #(= \1 %) conversion))))


;;; int-embedded instructions

(defn a-embedded
  [i]
  (fn [state] 
    (push-item (nth (first (:auxiliary state)) i)
               :boolean state)))

(define-registered a0 (a-embedded 0))
(define-registered a1 (a-embedded 1))
(define-registered a2 (a-embedded 2))

(defn d-embedded
  [i]
  (fn [state] 
    (push-item (nth (second (:auxiliary state)) i)
               :boolean state)))

(define-registered d0 (d-embedded 0))
(define-registered d1 (d-embedded 1))
(define-registered d2 (d-embedded 2))
(define-registered d3 (d-embedded 3))
(define-registered d4 (d-embedded 4))
(define-registered d5 (d-embedded 5))
(define-registered d6 (d-embedded 6))
(define-registered d7 (d-embedded 7))

(defn bits->int
  [bits]
  (loop [remaining bits total 0]
    (if (empty? remaining)
      total
      (recur (drop 1 remaining)
             (+ total (* (if (first remaining) 1 0) (expt 2 (dec (count remaining)))))))))
  
#_(pushgp 
    :error-function (fn [program]
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
                       (list 
                         (tag-instruction-erc [:exec] 1000)
                         (tagged-instruction-erc 1000))
                       '(a d exec_if boolean_and boolean_or boolean_not
                           ;boolean_dup boolean_swap boolean_pop boolean_rot
                           ;integer_add integer_sub integer_mult integer_div integer_mod
                           ;integer_dup integer_swap integer_pop integer_rot
                           ))
    :population-size 1000
    :max-points 50
    :mutation-probability 0.4
    :crossover-probability 0.4
    :simplification-probability 0.1
    :reproduction-simplifications 10
    :tournament-size 1
    :decimation-ratio 0.1
    :decimation-tournament-size 2)

(pushgp 
  :error-function (fn [program]
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
                     ;(list ;(fn [] (rand-int number-of-data-bits))
                     (repeat 4 (tag-instruction-erc [:exec] 1000))
                     (repeat 4 (tagged-instruction-erc 1000))
                     ;)
                     '(exec_if boolean_and boolean_or boolean_not
                               a0 a1 ;a2
                               d0 d1 d2 d3 ;d4 d5 d6 d7
                               ;boolean_dup boolean_swap boolean_pop boolean_rot
                               ))
  :population-size 5000
  :max-points 50
  :evalpush-limit 100
  :mutation-probability 0.4
  :crossover-probability 0.4
  :simplification-probability 0.1
  :reproduction-simplifications 1
  :node-selection-method :size-tournament
  :node-selection-tournament-size 2
  ;:tournament-size 1
  ;:decimation-ratio 0.1
  ;:decimation-tournament-size 2
  )
