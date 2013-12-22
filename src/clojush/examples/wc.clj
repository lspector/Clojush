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
        [clojush pushstate interpreter random util]
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
(defn wc-input
  "Makes a WC input of length len."
  [len]
  (apply str
         (repeatedly len
                     (fn []
                       (let [rand-cond (lrand)]
                         (cond
                           (< rand-cond 0.05) \tab
                           (< rand-cond 0.1) \newline
                           (< rand-cond 0.35) \space
                           :else (lrand-nth (map char (range 32 127)))))))))

;; A list of data domains for the WC problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def wc-data-domains
  [[(list "", "A", " ", "\t", "\n", "5", "#", "A\n", " \n", "\t\n", "\n\n",
          (apply str (repeat 100 \newline))
          (apply str (repeat 100 \space))
          (apply str (repeat 100 \tab))
          (apply str (repeat 100 \A))
          (apply str (take 100 (cycle (list \A \newline))))
          (apply str (take 100 (cycle (list \B \newline \newline))))
          (apply str (take 100 (cycle (list \C \D \newline))))
          (apply str (take 100 (cycle (list \E \space))))
          (apply str (take 100 (cycle (list \F \tab))))
          (apply str (take 100 (cycle (list \x \newline \y \space))))
          (apply str (take 100 (cycle (list \space \newline))))) 22 0] ;; "Special" inputs covering most base cases
   [(fn [] (str (wc-input (inc (lrand-int 99))) \newline)) 20 50] ;; Inputs ending in a newline
   [(fn [] (wc-input (inc (lrand-int 100)))) 200 500] ;; Inputs that may or may not end in a newline
   ])

(defn test-and-train-data-from-domains
  "Takes a list of domains and creates a set of (random) train inputs and a set of test
   inputs based on the domains. Returns [train test]. A program should not
   be considered a solution unless it is perfect on both the train and test
   cases."
  [domains]
  (apply mapv concat (map (fn [[input-set n-train n-test]]
                            (if (fn? input-set)
                              (vector (repeatedly n-train input-set)
                                      (repeatedly n-test input-set))
                              (vector (if (>= n-train (count input-set))
                                        input-set
                                        (take n-train (shuffle input-set)))
                                      (if (>= n-test (count input-set))
                                        input-set
                                        (take n-test (shuffle input-set))))))
                          domains)))

;;WC test data like this:
;(test-and-train-data-from-domains wc-data-domains)

;(defn random-wc-inputs
;  "Returns a list of n random inputs. Each will have size between 0 and 100."
;  [n]
;  (let [chars (map char (range 32 127))
;        max-len 100]
;    (repeatedly n
;                (partial wc-input (lrand-int (inc max-len))))))

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
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output-char output-word output-line]."
  [inputs]
  (map #(vector %
                (wc-char-count %)
                (wc-word-count %)
                (wc-line-count %))
       inputs))

;; Define error function. For now, each run uses different random inputs
(defn wc-error-function
  "Returns the error function for the wc problem. Takes as
   input WC data domains."
  [data-domains]
  (let [[train-cases test-cases] (map wc-test-cases
                                      (test-and-train-data-from-domains data-domains))]
    (doseq [[i [string ch wo li]] (map vector (range) train-cases)]
      (println (str "Train Case " i " | Chars: " ch " Words: " wo " Lines: " li))
      (println string)
      (println "--------------------"))
    (doseq [[i [string ch wo li]] (map vector (range) test-cases)]
      (println (str "Test Case " i " | Chars: " ch " Words: " wo " Lines: " li))
      (println string)
      (println "--------------------"))
    (fn the-actual-wc-error-function
      ([program]
        (the-actual-wc-error-function program :train))
      ([program data-cases] ;; data-cases should be :train or :test
        (flatten
          (doall
            (for [[input out-char out-word out-line] (case data-cases
                                                       :train train-cases
                                                       :test test-cases
                                                       [])]
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
                          100000))))))))))

(defn wc-report
  "Customize generational report."
  [best population generation error-function report-simplifications]
  (let [best-program (not-lazy (:program best))
        best-test-errors (error-function best-program :test)
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- WC problem report generation %s\n" generation)(flush)
    (println "Test total error for best:" best-total-test-error)
    (println (format "Test mean error for best: %.5f" (double (/ best-total-test-error (count best-test-errors)))))
    (when (zero? (:total-error best))
      (println "Test errors for best:" best-test-errors))
    (println ";;******************************")
    )) ;; To do validation, could have this function return an altered best individual
       ;; with total-error > 0 if it had error of zero on train but not on validation
       ;; set. Would need a third category of data cases.

; Define the argmap
(def argmap
  {:error-function (wc-error-function wc-data-domains)
   :atom-generators wc-atom-generators
   :max-points 1000
   :max-points-in-initial-program 400
   :evalpush-limit 2000
   :population-size 1000
   :max-generations 300
   :reproduction-probability 0
   :mutation-probability 0
   :crossover-probability 0
   :report-simplifications 0
   :ultra-probability 1.0
   :ultra-alternation-rate 0.01
   :ultra-alignment-deviation 10
   :ultra-mutation-rate 0.01
   :final-report-simplifications 5000
   :use-lexicase-selection true
   :use-ultra-no-paren-mutation true
   :problem-specific-report wc-report
   })
