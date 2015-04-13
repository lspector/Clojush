;; autoconstructive_digital_multiplier.clj
;; a version of digital-multiplier.clj modified to use only the autoconstriction
;; genetic operator.

;; Documentation for the original digital-multiplier.clj file:
;; digital-multiplier.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; This is code for the digital multiplier problem, as defined in:
;;    Walker, J.A.; Miller, J.F., "The Automatic Acquisition, Evolution and
;;    Reuse of Modules in Cartesian Genetic Programming," Evolutionary
;;    Computation, IEEE Transactions on , vol.12, no.4, pp.397,417, Aug. 2008
;;
;; The n-bit digital multiplier takes 2 n-bit numbers and outputs their
;; product. So, this problem requires 2*n bits of input and 2*n bits of output.
;; The input is stored in a vector as the first item on the auxiliary stack, and
;; the output is stored in a vector as the second item on the auxiliary stack.
;; Each of these vectors has 2*n items, accessed by instructions in0 through
;; in(2*n) and out0 through out(2*n) respectively.


;;; TO DO
;; epigenetic markers, and genome instructions for modifying them
;; exec instructions.... what does this mean for epigenetic markers to get the closes?
;; geography?



(ns clojush.problems.demos.autoconstructive-digital-multiplier
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random]
        clojure.math.numeric-tower))

;; Borrowed from mux examples
(defn int->bits-unmemoized
  [i num-bits]
  (let [conversion (Integer/toString i 2)]
    (concat (repeat (- num-bits (count conversion)) false)
            (map #(= \1 %) conversion))))

(def int->bits (memoize int->bits-unmemoized))

;; Define input and output instructions and symbols
(defn in
  [i]
  (fn [state] 
    (push-item (nth (stack-ref :auxiliary 1 state) i)
               :boolean state)))

(defn in-symbols
  [num-bits]
  (map #(vector % (symbol (str "in_dm" %))) (range num-bits)))

(defn define-ins
  [num-bits]
  (doseq [[num symb] (in-symbols num-bits)]
    (eval `(define-registered ~symb (in ~num)))))

(defn out
  [i]
  (fn [state]
    (if (empty? (:boolean state))
      state
      (push-item (assoc (top-item :auxiliary state)
                        i
                        (top-item :boolean state))
                 :auxiliary
                 (pop-item :auxiliary
                           (pop-item :boolean state))))))

(defn out-symbols
  [num-bits]
  (map #(vector % (symbol (str "out_dm" %))) (range num-bits)))

(defn define-outs
  [num-bits]
  (doseq [[num symb] (out-symbols num-bits)]
    (eval `(define-registered ~symb (out ~num)))))
      
;; Define atom generators for the problem
(defn dm-atom-generators
  [num-bits-n]
  (list
    (fn [] (lrand-nth (list 'boolean_and
                            'boolean_or
                            'boolean_xor
                            'boolean_invert_first_then_and
                            'boolean_dup
                            'boolean_swap
                            'boolean_rot)))
    (fn [] (lrand-nth (concat (map second (in-symbols (* 2 num-bits-n)))
                              (map second (out-symbols (* 2 num-bits-n))))))
    (fn [] (- (lrand-int 101) 50))
    (fn [] (lrand-nth (list 'integer_add
                            'integer_sub
                            'integer_mult
                            'integer_div
                            'integer_dup
                            'integer_swap
                            'integer_rot
                            'autoconstructive_integer_rand)))
    (fn [] (lrand-nth (list 'genome_gene_dup
                            'genome_gene_delete
                            'genome_rotate
                            'genome_gene_copy
                            'genome_toggle_silent
                            'genome_new
                            'genome_parent1
                            'genome_parent2
                            'genome_random
                            'genome_dup
                            'genome_swap
                            'genome_rot)))
    (fn [] (lrand-nth (list 'exec_do*range
                            'exec_do*count
                            'exec_do*times
                            'exec_while
                            'exec_do*while
                            'exec_if
                            'exec_when
                            'exec_k
                            'exec_s 
                            'exec_y)))
    )
  #_(concat (list (fn [] (lrand-nth (concat (map second (in-symbols (* 2 num-bits-n)))
                                           (map second (out-symbols (* 2 num-bits-n))))))
                 (fn [] (- (lrand-int 101) 50)))
           (registered-for-stacks [:integer :boolean :genome :exec]))
  )

;; Create test cases
(defn dm-test-cases
  [num-bits-n]
  (for [num1 (range (expt 2 num-bits-n))
        num2 (range (expt 2 num-bits-n))]
    (let [input-bits (vec (concat (int->bits num1 num-bits-n)
                                  (int->bits num2 num-bits-n)))
          output-bits (vec (int->bits (* num1 num2) (* 2 num-bits-n)))]
      (vector input-bits output-bits))))

;; Create error function; it is applied partially when defined, and takes
;; a program and returns its error vector.
;(defn dm-error-function
;  "Defines the error function of num-bits binary multiplier."
;  [num-bits-n test-cases program]
;  (doall
;    (for [[input output] test-cases]
;      (let [initial-output-vector (vec (repeat (* 2 num-bits-n) nil))
;            final-state (run-push program
;                                  (push-item initial-output-vector
;                                             :auxiliary 
;                                             (push-item input
;                                                        :auxiliary
;                                                        (make-push-state))))
;            result-output (top-item :auxiliary final-state)]
;        ; For each bit, correct contributes 0 to error, incorrect
;        ; contributes 1 to error, and nil (i.e. that bit was never
;        ; output) contributes 1 to error (but, it could have a
;        ; larger penalty if desired).
;        (apply + (map (fn [expected-bit out-bit]
;                        (cond
;                          (nil? out-bit) 1
;                          (= expected-bit out-bit) 0
;                          :else 1))
;                      output
;                      result-output))))))

;; Create error function; it is applied partially when defined, and takes
;; a program and returns its error vector.
(defn dm-error-function
  "Defines the error function of num-bits binary multiplier."
  [num-bits-n test-cases individual]
  (let [program (:program individual)
        errors (doall
                 (for [[input output] test-cases]
                   (let [initial-output-vector (vec (repeat (* 2 num-bits-n) nil))
                         final-state (run-push program
                                               (push-item initial-output-vector
                                                          :auxiliary 
                                                          (push-item input
                                                                     :auxiliary
                                                                     (make-push-state))))
                         result-output (top-item :auxiliary final-state)]
                     ; For each bit, correct contributes 0 to error, incorrect
                     ; contributes 1 to error, and nil (i.e. that bit was never
                     ; output) contributes 1 to error (but, it could have a
                     ; larger penalty if desired).
                     (apply + (map (fn [expected-bit out-bit]
                                     (cond
                                       (nil? out-bit) 1
                                       (= expected-bit out-bit) 0
                                       :else 1))
                                   output
                                   result-output)))))]
    (conj errors 
          (if (:random-replacement-for-clone individual)
            0.5
            (if (some #{errors} (map rest (:parent-errors individual)))
              0.1
              0)))))

;; Define argmap for pushgp
(defn define-digital-multiplier
  [num-bits-n]
  (define-ins (* 2 num-bits-n))
  (define-outs (* 2 num-bits-n))
  (def argmap
    {:error-function (partial dm-error-function
                              num-bits-n
                              (dm-test-cases num-bits-n))
     :atom-generators (dm-atom-generators num-bits-n)
     :population-size 2000
     :max-generations 2000
     :max-points 500
     :max-points-in-initial-program 100
     :evalpush-limit 5000
     :epigenetic-markers [:close :silent]
     :genetic-operator-probabilities {:autoconstruction 1}
     :parent-selection :lexicase
     ;:trivial-geography-radius 50
     :report-simplifications 0
     :pass-individual-to-error-function true}
    )
  )

;; Create the argmap passing the number of bits for the problem
(define-digital-multiplier 3)
