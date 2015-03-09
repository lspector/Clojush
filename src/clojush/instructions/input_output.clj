(ns clojush.instructions.input-output
  (:use [clojush pushstate globals util]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; instructions for inputs and outputs, including printing

(defn printer 
  "Returns a function that takes a state and prints the top item of the
   appropriate stack of the state."
  [type]
  (fn [state]
    (if (empty? (type state))
      state
      (let [top-thing (top-item type state)
            top-thing-string (cond
                               (or (string? top-thing)
                                   (char? top-thing)) top-thing
                               (float? top-thing) (pr-str (round-to-n-decimal-places top-thing 10))
                               :else (pr-str top-thing))]
        (if (< max-string-length (count (str (stack-ref :output 0 state) top-thing-string)))
          state
          (stack-assoc (str (stack-ref :output 0 state) top-thing-string)
                       :output
                       0
                       (pop-item type state)))))))

(define-registered print_exec (with-meta (printer :exec) {:stack-types [:print :exec] :parentheses 1}))
(define-registered print_integer (with-meta (printer :integer) {:stack-types [:print :integer]}))
(define-registered print_float (with-meta (printer :float) {:stack-types [:print :float]}))
(define-registered print_code (with-meta (printer :code) {:stack-types [:print :code]}))
(define-registered print_boolean (with-meta (printer :boolean) {:stack-types [:print :boolean]}))
(define-registered print_string (with-meta (printer :string) {:stack-types [:print :string]}))
(define-registered print_char (with-meta (printer :char) {:stack-types [:print :char]}))
(define-registered print_vector_integer (with-meta (printer :vector_integer) {:stack-types [:print :vector_integer]}))
(define-registered print_vector_float (with-meta (printer :vector_float) {:stack-types [:print :vector_float]}))
(define-registered print_vector_boolean (with-meta (printer :vector_boolean) {:stack-types [:print :vector_boolean]}))
(define-registered print_vector_string (with-meta (printer :vector_string) {:stack-types [:print :vector_string]}))
;(define-registered print_zip (with-meta (printer :zip) {:stack-types [:print :zip]})) ; I don't think we want this

(define-registered
  print_newline
  ^{:stack-types [:print]}
  (fn [state]
    (if (< max-string-length (count (str (stack-ref :output 0 state) \newline)))
      state
      (stack-assoc (str (stack-ref :output 0 state) \newline)
                   :output
                   0
                   state))))

(defn handle-input-instruction
  "Allows Push to handle inN instructions, e.g. in2, using things from the input
   stack. We can tell whether a particular inN instruction is valid if N-1
   values are on the input stack. Recognizes vectors, simple literals and quoted code."
  [instr state]
  (let [n (Integer/parseInt (second (first (re-seq #"in(\d+)" (name instr)))))]
    (if (or (> n (count (:input state)))
            (< n 1))
      (throw (Exception. (str "Undefined instruction: " (pr-str instr) "\nNOTE: Likely not same number of items on input stack as input instructions.")))
      (let [item (stack-ref :input (dec n) state)
            literal-type (recognize-literal item)]
        (cond
          (and (vector? item) (= [] item)) (push-item [] :vector_integer (push-item [] :vector_float (push-item [] :vector_string (push-item [] :vector_boolean state))))
          (seq? item) (push-item item :exec state)
          :else (push-item item literal-type state))))))
