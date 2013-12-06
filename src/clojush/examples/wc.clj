;; wc.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; This is code for the problem emulating the "wc" unix command,
;; which returns the character, word, and line counts of a file.
;; It uses faked IO instructions such as string_readchar
;; and string_readline to read strings from the file and place them
;; on the string stack. The end of file happens when the file string
;; on the :auxiliary stack is empty, which can be checked with the
;; file_EOF instruction. Output is an integer that comes from one
;; of the three output instructions.
;;

;; NOTE: A word is defined by any characters separated by any number
;; of spaces, tabs, and newlines. Any other character can be part of
;; a word.

;; NOTE: auxiliary stack has, from top: working_file, full_file,
;; char_count_out, word_count_out, line_count_out

(ns clojush.examples.wc
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random]
        clojush.instructions.tag
        clojure.math.numeric-tower
        [clojure.string :only [split trim]]))

; Hand-coded solution for just character count
#_(run-push '(exec_y (string_readchar file_EOF exec_when exec_pop) string_stackdepth output_charcount)
          (->> (make-push-state)
               (push-item nil :auxiliary)
               (push-item nil :auxiliary)
               (push-item nil :auxiliary)
               (push-item "hello\nworld" :auxiliary)
               (push-item "hello\nworld" :auxiliary)))

; Define new instructions
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
  string_whitespace ;;returns true if top string is entirely composed of spaces, tabs, and newlines (even if empty)
  (fn [state]
    (if (not (empty? (:string state)))
      (push-item (every? #{\space \tab \newline} (top-item :string state))
                 :boolean
                 (pop-item :string state))
      state)))

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

(define-registered
  output_charcount
  (fn [state]
    (if (empty? (:integer state))
      state
      (let [top-int (top-item :integer state)]
        (stack-assoc top-int :auxiliary 2
                     (pop-item :integer state))))))

(define-registered
  output_wordcount
  (fn [state]
    (if (empty? (:integer state))
      state
      (let [top-int (top-item :integer state)]
        (stack-assoc top-int :auxiliary 3
                     (pop-item :integer state))))))

(define-registered
  output_linecount
  (fn [state]
    (if (empty? (:integer state))
      state
      (let [top-int (top-item :integer state)]
        (stack-assoc top-int :auxiliary 4
                     (pop-item :integer state))))))

; Make atom generators
(def wc-atom-generators
  (list
    (fn [] (- (lrand-int 201) 100))
    (fn [] (lrand-nth ["\n" "\t" " "]))
    (fn [] (lrand-nth (concat ["\n" "\t"] (map (comp str char) (range 32 127)))))
    (tag-instruction-erc [:exec :string :integer] 1000)
    (tagged-instruction-erc 1000)
    ;;;; end ERCs
    'string_readchar
    'string_readline
    'string_whitespace
    'file_EOF
    'file_begin
    'output_charcount
    'output_wordcount
    'output_linecount
    ;;;; end problem-specific instructions
    'string_pop
    'string_take
    'string_eq
    'string_stackdepth
    'string_rot
    'string_parse_to_chars
    ;'string_rand
    'string_contained
    'string_reverse
    'string_yank
    'string_swap
    'string_yankdup
    'string_flush
    'string_length
    'string_concat
    ;'string_atoi
    'string_shove
    'string_dup
    'string_split
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
(defn random-wc-inputs
  "Returns a list of n random inputs. Each will have size between 0 and 100."
  [n]
  (let [chars (map char (range 32 127))
        max-len 100]
    (repeatedly n
                (fn []
                  (apply str
                         (repeatedly (lrand-int (inc max-len))
                                     (fn []
                                       (let [rand-cond (lrand)]
                                         (cond
                                           (< rand-cond 0.05) \tab
                                           (< rand-cond 0.1) \newline
                                           (< rand-cond 0.35) \space
                                           :else (lrand-nth chars))))))))))

(defn wc-char-count
  "Takes a wc input and returns the char count output"
  [input]
  (count input))

(defn wc-word-count
  "Takes a wc input and returns the word count output"
  [input]
  (count (filter not-empty (split (trim input) #"\s+"))))
    
(defn wc-line-count
  "Takes a wc input and returns the line count output. Only lines ending
   newlines count toward total."
  [input]
  (get (frequencies input) \newline 0))

(defn wc-test-cases
  "Gives n IO test cases of the form [input output-char output-word output-line]."
  [n]
  (map #(vector %
                (wc-char-count %)
                (wc-word-count %)
                (wc-line-count %))
       (random-wc-inputs n)))

;; Define error function. For now, each run uses different random inputs
(defn wc-error-function
  "Returns the error function for the wc problem. Takes as
   input number of test cases to use."
  ([number-test-cases]
    (wc-error-function number-test-cases
                       (wc-test-cases number-test-cases)))
  ([_ test-cases]
    (fn [program]
      (flatten
        (doall
          (for [[input out-char out-word out-line] test-cases]
            (let [final-state (run-push program
                                        (->> (make-push-state)
                                             (push-item nil :auxiliary)
                                             (push-item nil :auxiliary)
                                             (push-item nil :auxiliary)
                                             (push-item input :auxiliary)
                                             (push-item input :auxiliary)))
                  result-char (stack-ref :auxiliary 2 final-state)
                  result-word (stack-ref :auxiliary 3 final-state)
                  result-line (stack-ref :auxiliary 4 final-state)]
              ; The error is the integer difference between the desired output
              ; and the result output for each of char-count, word-count, and
              ; line-count. Because of this, each test case results in 3
              ; error values. If a value is missing, a penalty of 100000
              ; is given.
              (vector (if (number? result-char)
                        (abs (- result-char out-char))
                        100000)
                      (if (number? result-word)
                        (abs (- result-word out-word))
                        100000)
                      (if (number? result-line)
                        (abs (- result-line out-line))
                        100000)))))))))

; Define the argmap
(def argmap
  {:error-function (wc-error-function 50)
   :atom-generators wc-atom-generators
   :max-points 200
   :max-points-in-initial-program 50
   :evalpush-limit 1000
   :population-size 500
   :max-generations 200
   :reproduction-probability 0
   :mutation-probability 0
   :crossover-probability 0
   :report-simplifications 0
   :ultra-probability 1.0
   :final-report-simplifications 1000
   :use-lexicase-selection true
   :use-ultra-no-paren-mutation true
   })
