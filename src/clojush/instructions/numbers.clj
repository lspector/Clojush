(ns clojush.instructions.numbers
  (:use [clojush.pushstate]
        [clojush.util])
  (:require [clojure.math.numeric-tower :as nt]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; instructions for numbers

(defn adder
  "Returns a function that pushes the sum of the top two items."
  [type]
  (fn [state]
    (if (not (empty? (rest (type state))))
      (let [first (stack-ref type 0 state)
            second (stack-ref type 1 state)]
        (->> (pop-item type state)
             (pop-item type)
             (push-item (keep-number-reasonable (+' first second)) type)))
      state)))

(define-registered integer_add (with-meta (adder :integer) {:stack-types [:integer]}))
(define-registered float_add (with-meta (adder :float) {:stack-types [:float]}))

(defn subtracter
  "Returns a function that pushes the difference of the top two items."
  [type]
  (fn [state]
    (if (not (empty? (rest (type state))))
      (let [first (stack-ref type 0 state)
            second (stack-ref type 1 state)]
        (->> (pop-item type state)
             (pop-item type)
             (push-item (keep-number-reasonable (- second first)) type)))
      state)))

(define-registered integer_sub (with-meta (subtracter :integer) {:stack-types [:integer]}))
(define-registered float_sub (with-meta (subtracter :float) {:stack-types [:float]}))

(defn multiplier
  "Returns a function that pushes the product of the top two items."
  [type]
  (fn [state]
    (if (not (empty? (rest (type state))))
      (let [first (stack-ref type 0 state)
            second (stack-ref type 1 state)]
        (->> (pop-item type state)
             (pop-item type)
             (push-item (keep-number-reasonable (*' second first)) type)))
      state)))

(define-registered integer_mult (with-meta (multiplier :integer) {:stack-types [:integer]}))
(define-registered float_mult (with-meta (multiplier :float) {:stack-types [:float]}))

(defn divider
  "Returns a function that pushes the quotient of the top two items. Does
   nothing if the denominator would be zero."
  [type]
  (fn [state]
    (if (and (not (empty? (rest (type state))))
             (not (zero? (stack-ref type 0 state))))
      (let [first (stack-ref type 0 state)
            second (stack-ref type 1 state)]
        (->> (pop-item type state)
             (pop-item type)
             (push-item (if (= type :integer)
                          (truncate (keep-number-reasonable (/ second first)))
                          (keep-number-reasonable (/ second first)))
                        type)))
      state)))

(define-registered integer_div (with-meta (divider :integer) {:stack-types [:integer]}))
(define-registered float_div (with-meta (divider :float) {:stack-types [:float]}))

(defn modder
  "Returns a function that pushes the modulus of the top two items. Does
   nothing if the denominator would be zero."
  [type]
  (fn [state]
    (if (and (not (empty? (rest (type state))))
             (not (zero? (stack-ref type 0 state))))
      (let [frst (stack-ref type 0 state)
            scnd (stack-ref type 1 state)]
        (->> (pop-item type state)
             (pop-item type)
             (push-item (if (= type :integer)
                          (truncate (keep-number-reasonable (mod scnd frst)))
                          (keep-number-reasonable (mod scnd frst)))
                        type)))
      state)))

(define-registered integer_mod (with-meta (modder :integer) {:stack-types [:integer]}))
(define-registered float_mod (with-meta (modder :float) {:stack-types [:float]}))

(defn comparer
  "Returns a function that pushes the result of comparator of the top two items
   on the 'type' stack onto the boolean stack."
  [type comparator]
  (fn [state]
    (if (not (empty? (rest (type state))))
      (let [first (stack-ref type 0 state)
            second (stack-ref type 1 state)]
        (->> (pop-item type state)
             (pop-item type)
             (push-item (comparator second first) :boolean)))
      state)))

(define-registered integer_lt (with-meta (comparer :integer <) {:stack-types [:integer :boolean]}))
(define-registered integer_lte (with-meta (comparer :integer <=) {:stack-types [:integer :boolean]}))
(define-registered integer_gt (with-meta (comparer :integer >) {:stack-types [:integer :boolean]}))
(define-registered integer_gte (with-meta (comparer :integer >=) {:stack-types [:integer :boolean]}))
(define-registered float_lt (with-meta (comparer :float <) {:stack-types [:float :boolean]}))
(define-registered float_lte (with-meta (comparer :float <=) {:stack-types [:float :boolean]}))
(define-registered float_gt (with-meta (comparer :float >) {:stack-types [:float :boolean]}))
(define-registered float_gte (with-meta (comparer :float >=) {:stack-types [:float :boolean]}))

(define-registered
  integer_fromboolean
  ^{:stack-types [:integer :boolean]}
  (fn [state]
    (if (not (empty? (:boolean state)))
      (let [item (stack-ref :boolean 0 state)]
        (->> (pop-item :boolean state)
             (push-item (if item 1 0) :integer)))
      state)))

(define-registered
  float_fromboolean
  ^{:stack-types [:float :boolean]}
  (fn [state]
    (if (not (empty? (:boolean state)))
      (let [item (stack-ref :boolean 0 state)]
        (->> (pop-item :boolean state)
             (push-item (if item 1.0 0.0) :float)))
      state)))

(define-registered
  integer_fromfloat
  ^{:stack-types [:integer :float]}
  (fn [state]
    (if (not (empty? (:float state)))
      (let [item (stack-ref :float 0 state)]
        (->> (pop-item :float state)
             (push-item (truncate item) :integer)))
      state)))

(define-registered
  float_frominteger
  ^{:stack-types [:integer :float]}
  (fn [state]
    (if (not (empty? (:integer state)))
      (let [item (stack-ref :integer 0 state)]
        (->> (pop-item :integer state)
             (push-item (*' 1.0 item) :float)))
      state)))

(define-registered
  integer_fromstring
  ^{:stack-types [:integer :string]}
  (fn [state]
    (if (not (empty? (:string state)))
      (try (pop-item :string
                     (push-item (keep-number-reasonable (Long/parseLong (top-item :string state)))
                                :integer state))
        (catch Exception e state))
      state)))

(define-registered
  float_fromstring
  ^{:stack-types [:float :string]}
  (fn [state]
    (if (not (empty? (:string state)))
      (try (pop-item :string
                     (push-item (keep-number-reasonable (Float/parseFloat (top-item :string state)))
                                :float state))
        (catch Exception e state))
      state)))

(define-registered
  integer_fromchar
  ^{:stack-types [:integer :char]}
  (fn [state]
    (if (not (empty? (:char state)))
      (let [item (stack-ref :char 0 state)]
        (->> (pop-item :char state)
             (push-item (int item) :integer)))
      state)))

(define-registered
  float_fromchar
  ^{:stack-types [:float :char]}
  (fn [state]
    (if (not (empty? (:char state)))
      (let [item (stack-ref :char 0 state)]
        (->> (pop-item :char state)
             (push-item (float (int item)) :float)))
      state)))

(defn minner
  "Returns a function that pushes the minimum of the top two items."
  [type]
  (fn [state]
    (if (not (empty? (rest (type state))))
      (let [first (stack-ref type 0 state)
            second (stack-ref type 1 state)]
        (->> (pop-item type state)
             (pop-item type)
             (push-item (min second first) type)))
      state)))

(define-registered integer_min (with-meta (minner :integer) {:stack-types [:integer]}))
(define-registered float_min (with-meta (minner :float) {:stack-types [:float]}))

(defn maxer
  "Returns a function that pushes the maximum of the top two items."
  [type]
  (fn [state]
    (if (not (empty? (rest (type state))))
      (let [first (stack-ref type 0 state)
            second (stack-ref type 1 state)]
        (->> (pop-item type state)
             (pop-item type)
             (push-item (max second first) type)))
      state)))

(define-registered integer_max (with-meta (maxer :integer) {:stack-types [:integer]}))
(define-registered float_max (with-meta (maxer :float) {:stack-types [:float]}))

(define-registered
  float_sin
  ^{:stack-types [:float]}
  (fn [state]
    (if (not (empty? (:float state)))
      (push-item (keep-number-reasonable (Math/sin (stack-ref :float 0 state)))
                 :float
                 (pop-item :float state))
      state)))

(define-registered
  float_cos
  ^{:stack-types [:float]}
  (fn [state]
    (if (not (empty? (:float state)))
      (push-item (keep-number-reasonable (Math/cos (stack-ref :float 0 state)))
                 :float
                 (pop-item :float state))
      state)))

(define-registered
  float_tan
  ^{:stack-types [:float]}
  (fn [state]
    (if (not (empty? (:float state)))
      (push-item (keep-number-reasonable (Math/tan (stack-ref :float 0 state)))
                 :float
                 (pop-item :float state))
      state)))

(define-registered
  integer_inc
  ^{:stack-types [:integer]}
  (fn [state]
    (if (not (empty? (:integer state)))
      (push-item (keep-number-reasonable (inc (stack-ref :integer 0 state)))
                 :integer
                 (pop-item :integer state))
      state)))

(define-registered
  integer_dec
  ^{:stack-types [:integer]}
  (fn [state]
    (if (not (empty? (:integer state)))
      (push-item (keep-number-reasonable (dec (stack-ref :integer 0 state)))
                 :integer
                 (pop-item :integer state))
      state)))

(define-registered
  float_inc
  ^{:stack-types [:float]}
  (fn [state]
    (if (not (empty? (:float state)))
      (push-item (keep-number-reasonable (inc (stack-ref :float 0 state)))
                 :float
                 (pop-item :float state))
      state)))

(define-registered
  float_dec
  ^{:stack-types [:float]}
  (fn [state]
    (if (not (empty? (:float state)))
      (push-item (keep-number-reasonable (dec (stack-ref :float 0 state)))
                 :float
                 (pop-item :float state))
      state)))

(defn negater
  "Returns a function that pushes the negation of the top item."
  [type]
  (fn [state]
    (if (not (empty? (type state)))
      (push-item (keep-number-reasonable (- (stack-ref type 0 state)))
                 type
                 (pop-item type state))
      state)))

(define-registered integer_negate (with-meta (negater :integer) {:stack-types [:integer]}))
(define-registered float_negate (with-meta (negater :float) {:stack-types [:float]}))

(defn abser
  "Returns a function that pushes the absolute value of the top item."
  [type]
  (fn [state]
    (if (not (empty? (type state)))
      (let [num (stack-ref type 0 state)]
        (push-item (keep-number-reasonable (if (neg? num) (- num) num))
                   type
                   (pop-item type state)))
      state)))

(define-registered integer_abs (with-meta (abser :integer) {:stack-types [:integer]}))
(define-registered float_abs (with-meta (abser :float) {:stack-types [:float]}))

(defn safe-expt
  "Fast and safe implementation of expt that ignores very large and
   very negative exponents. Also treats 0 and 1 bases correctly.
   Also treats integer base and negative exponent correctly."
  [base exp type]
  (let [result (cond ; handle cases where this is very slow because of large exponents
                 (zero? base) 0
                 (= 1 base) 1
                 (and (> exp 40)
                      (>= base 2)) 100000000000000N
                 (and (> exp 40)
                      (<= base -2)) (if (zero? (mod exp 2)) 100000000000000N -100000000000000N)
                 (< exp -40) 0
                 (and (= type :integer)
                      (< exp 0)) 0
                 :else (keep-number-reasonable (nt/expt base exp)))]
    (if (= type :float)
      (float result)
      result)))

(defn power
  "Returns a function that pushes the top value of the stack raised to the power of the second value"
  [type]
  (fn [state]
    (if (not (empty? (rest (type state))))
      (let [base (stack-ref type 1 state)
            exp (stack-ref type 0 state)
            result (safe-expt base exp type)]
        (push-item (keep-number-reasonable result)
                   type
                   (pop-item type (pop-item type state))))
      state)))

(define-registered integer_pow (with-meta (power :integer) {:stack-types [:integer]}))
(define-registered float_pow (with-meta (power :float) {:stack-types [:float]}))

(defn squareer
  "Returns a function that pushes the top item squared."
  [type]
  (fn [state]
    (if (not (empty? (type state)))
      (let [num (stack-ref type 0 state)]
        (push-item (keep-number-reasonable (*' num num))
                   type
                   (pop-item type state)))
      state)))

(define-registered float_square (with-meta (squareer :float) {:stack-types [:float]}))

(defn sqrter
  "Returns a function that pushes the square root of the top item."
  [type]
  (fn [state]
    (if (not (empty? (type state)))
      (let [num (stack-ref type 0 state)]
        (push-item (keep-number-reasonable (nt/sqrt num))
                   type
                   (pop-item type state)))
      state)))

(define-registered float_sqrt (with-meta (sqrter :float) {:stack-types [:float]}))

(defn log2
  "Takes log_2(x)"
  [x]
  (/ (Math/log x)
     (Math/log 2)))

(defn loger
  "Returns a function that pushes the log (base 10) of the top item."
  [type base]
  (fn [state]
    (if (not (empty? (type state)))
      (let [num (stack-ref type 0 state)]
        (push-item (keep-number-reasonable (if (= base 10)
                                             (Math/log10 num)
                                             (log2 num)))
                   type
                   (pop-item type state)))
      state)))

(define-registered float_log10 (with-meta (loger :float 10) {:stack-types [:float]}))
(define-registered float_log2 (with-meta (loger :float 2) {:stack-types [:float]}))

(defn ceilinger
  "Returns a function that pushes the ceiling of the top item."
  [type]
  (fn [state]
    (if (not (empty? (type state)))
      (let [num (stack-ref type 0 state)]
        (push-item (keep-number-reasonable (nt/ceil num))
                   type
                   (pop-item type state)))
      state)))

(define-registered float_ceiling (with-meta (ceilinger :float) {:stack-types [:float]}))

(defn floorer
  "Returns a function that pushes the floor of the top item."
  [type]
  (fn [state]
    (if (not (empty? (type state)))
      (let [num (stack-ref type 0 state)]
        (push-item (keep-number-reasonable (nt/floor num))
                   type
                   (pop-item type state)))
      state)))

(define-registered float_floor (with-meta (floorer :float) {:stack-types [:float]}))

(defn arccoser
  "Returns a function that pushes the arccos of the top item."
  [type]
  (fn [state]
    (if (not (empty? (type state)))
      (let [num (stack-ref type 0 state)]
        (push-item (keep-number-reasonable (Math/acos num))
                   type
                   (pop-item type state)))
      state)))

(define-registered float_arccos (with-meta (arccoser :float) {:stack-types [:float]}))

(defn arcsiner
  "Returns a function that pushes the arcsin of the top item."
  [type]
  (fn [state]
    (if (not (empty? (type state)))
      (let [num (stack-ref type 0 state)]
        (push-item (keep-number-reasonable (Math/asin num))
                   type
                   (pop-item type state)))
      state)))

(define-registered float_arcsin (with-meta (arcsiner :float) {:stack-types [:float]}))

(defn arctaner
  "Returns a function that pushes the arctan of the top item."
  [type]
  (fn [state]
    (if (not (empty? (type state)))
      (let [num (stack-ref type 0 state)]
        (push-item (keep-number-reasonable (Math/atan num))
                   type
                   (pop-item type state)))
      state)))

(define-registered float_arctan (with-meta (arctaner :float) {:stack-types [:float]}))
