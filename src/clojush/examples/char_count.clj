;; char_count.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; This is code for the problem of counting the number of characters
;; in a file. It uses faked IO instructions such as string_readchar
;; and string_readline to read strings from the file and place them
;; on the string stack. The end of file happens when the file string
;; on the :auxiliary stack is empty, which can be checked with the
;; file_EOF instruction. Output is an integer from the integer stack.
;;

(ns clojush.examples.char-count
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random]
        clojush.instructions.tag
        clojure.math.numeric-tower))

; Hand-coded solution
#_(run-push '(exec_y (string_readchar file_EOF exec_when exec_pop) string_stackdepth)
          (push-item "hello\nworld" :auxiliary 
          (push-item "hello\nworld" :auxiliary (make-push-state))))

; Make atom generators
(define-registered
  string_readchar
  (fn [state]
    (let [file (top-item :auxiliary state)
          first-char (first file)
          aux-result (push-item (apply str (rest file))
                                :auxiliary
                                (pop-item :auxiliary state))]
      (if (= file "")
        state
        (push-item (str first-char)
                   :string
                   aux-result)))))

(define-registered
  string_readline
  (fn [state]
    (let [file (top-item :auxiliary state)
          index (inc (.indexOf file "\n"))
          has-no-newline (= 0 index)
          aux-result (push-item (if has-no-newline
                                  ""
                                  (subs file index))
                                :auxiliary
                                (pop-item :auxiliary state))]
      (if (= file "")
        state
        (if has-no-newline
          (push-item file :string aux-result)
          (push-item (subs file 0 index)
                     :string
                     aux-result))))))

(define-registered
  file_EOF
  (fn [state]
    (let [file (top-item :auxiliary state)
          result (empty? file)]
      (push-item result :boolean state))))

(define-registered
  file_begin
  (fn [state]
    (push-item (stack-ref :auxiliary 1 state)
               :auxiliary
               (pop-item :auxiliary state))))

(def char-count-atom-generators
  (list
    (fn [] (- (lrand-int 201) 100))
    (fn [] (lrand-nth (cons "\n" (map (comp str char) (range 32 127)))))
    ;;;; end ERCs
    'string_readchar
    'string_readline
    'file_EOF
    'file_begin
    ;;;; end problem-specific string instructions
    'string_pop
    'string_take
    'string_eq
    'string_stackdepth
    'string_rot
    'string_parse_to_chars
    ;'string_rand
    ;'string_contained
    ;'string_reverse
    'string_yank
    'string_swap
    'string_yankdup
    ;'string_flush
    'string_length
    'string_concat
    ;'string_atoi
    'string_shove
    'string_dup
    ;;; end string instructions
    'integer_add
    'integer_swap
    'integer_yank
    'integer_dup
    'integer_yankdup
    'integer_shove
    'integer_mult
    'integer_div
    'integer_max
    'integer_sub
    'integer_mod
    'integer_rot
    'integer_min
    'integer_inc
    'integer_dec
    ;;; end integer instructions
    'exec_y
    'exec_pop
    'exec_eq
    'exec_stackdepth
    'exec_rot
    'exec_when
    'exec_do*times
    'exec_do*count
    'exec_s
    'exec_do*range
    'exec_if
    'exec_k
    'exec_yank
    'exec_yankdup
    'exec_swap
    'exec_dup
    'exec_shove
    ;;; end exec instructions
    'boolean_swap
    'boolean_and
    'boolean_not
    'boolean_or
    'boolean_frominteger
    'boolean_stackdepth
    'boolean_dup))

;; Define test cases
(defn random-char-count-inputs
  "Returns a list of n random inputs. Each will have size between 0
   and 100 and will end with a (char 4) character."
  [n]
  (let [chars (map char (range 32 127))
        max-len 100]
    (repeatedly n
                (fn []
                  (apply str (repeatedly (lrand-int (inc max-len))
                                         (fn []
                                           (if (< (lrand) 0.1)
                                             \newline
                                             (lrand-nth chars)))))))))

(defn char-count-input-to-output
  "Takes a char-count input and returns the correct output"
  [input]
  (count input))

(defn char-count-test-cases
  "Gives n IO-pair test cases"
  [n]
  (map #(vector % (char-count-input-to-output %))
       (random-char-count-inputs n)))

;; Define error function
(defn char-count-error-function
  "Returns the error function for the char-count problem. Takes as
   input number of test cases to use."
  [number-test-cases]
  (fn [program]
    (doall
      (for [[input output] (char-count-test-cases number-test-cases)]
        (let [final-state (run-push program
                                    (push-item input
                                               :auxiliary
                                               (push-item input
                                                          :auxiliary
                                                          (make-push-state))))
              result-output (top-item :integer final-state)]
          ; The error is the integer difference between the desired output
          ; and the result output.
          (if (number? result-output)
            (abs (- result-output output))
            100000))))))

; Define the argmap
(def argmap
  {:error-function (char-count-error-function 50)
   :atom-generators char-count-atom-generators
   :max-points 100
   :max-points-in-initial-program 50
   :evalpush-limit 1000
   :population-size 500
   :max-generations 200
   :mutation-probability 0
   :crossover-probability 0
   :report-simplifications 0
   :ultra-probability 1.0
   :final-report-simplifications 1000
   :use-lexicase-selection true
   })
