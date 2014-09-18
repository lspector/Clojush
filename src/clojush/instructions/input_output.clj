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
            top-thing-string (if (or (string? top-thing)
                                     (char? top-thing))
                               top-thing
                               (pr-str top-thing))]
        (if (< max-string-length (count (str (stack-ref :output 0 state) top-thing-string)))
          state
          (stack-assoc (str (stack-ref :output 0 state) top-thing-string)
                       :output
                       0
                       (pop-item type state)))))))

(define-registered print_exec (printer :exec))
(define-registered print_integer (printer :integer))
(define-registered print_float (printer :float))
(define-registered print_code (printer :code))
(define-registered print_boolean (printer :boolean))
(define-registered print_string (printer :string))
(define-registered print_char (printer :char))
;(define-registered print_zip (printer :zip)) ; I don't think we want this

(define-registered
  print_newline
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
   values are on the input stack."
  [instr state]
  (let [n (Integer/parseInt (second (first (re-seq #"in(\d+)" (name instr)))))]
    (if (or (> n (count (:input state)))
            (< n 1))
      (throw (Exception. (str "Undefined instruction: " (pr-str instr) "\nNOTE: Likely not same number of items on input stack as input instructions.")))
      (let [item (stack-ref :input (dec n) state)
            literal-type (recognize-literal item)]
        (if (and (vector? item) (= [] item))
          (push-item [] :vector_integer (push-item [] :vector_float (push-item [] :vector_string (push-item [] :vector_boolean state))))
          (push-item item literal-type state))))))
