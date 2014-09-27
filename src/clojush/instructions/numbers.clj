(ns clojush.instructions.numbers
  (:use [clojush.pushstate]
        [clojush.util]))

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
