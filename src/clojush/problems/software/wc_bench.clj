;; wc_bench.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; This is code for the problem emulating the "wc" unix command,
;; which returns the character, word, and line counts of a file.
;; It uses faked IO instructions such as string_readchar
;; and string_readline to read strings from the file and place them
;; on the string stack. The end of file happens when the file string
;; on the :input stack is empty, which can be checked with the
;; file_EOF instruction. Output is an integer that comes from one
;; of the three output instructions.
;;
;; NOTE: A word is defined by any characters separated by any number
;; of spaces, tabs, and newlines. Any other character can be part of
;; a word.
;;
;; NOTE: input stack has, from top: working_file, full_file,
;; char_count_out, word_count_out, line_count_out

(ns clojush.problems.software.wc-bench
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower
        [clojure.string :only [split trim]]))

; Define new instructions
(define-registered
  file_readchar
  ^{:stack-types [:char]}
  (fn [state]
    (let [file (top-item :input state)
          first-char (first file)
          inp-result (push-item (apply str (rest file))
                                :input
                                (pop-item :input state))]
      (if (= file "")
        state
        (push-item first-char
                   :char
                   inp-result)))))

(define-registered
  file_readline
  ^{:stack-types [:string]}
  (fn [state]
    (let [file (top-item :input state)
          index (inc (.indexOf file "\n"))
          has-no-newline (= 0 index)
          aux-result (push-item (if has-no-newline
                                  ""
                                  (subs file index))
                                :input
                                (pop-item :input state))]
      (if (= file "")
        state
        (if has-no-newline
          (push-item file :string aux-result)
          (push-item (subs file 0 index)
                     :string
                     aux-result))))))

(define-registered 
  string_whitespace ;;returns true if top string is entirely composed of spaces, tabs, and newlines (even if empty)
  ^{:stack-types [:string]}
  (fn [state]
    (if (not (empty? (:string state)))
      (push-item (every? #{\space \tab \newline} (top-item :string state))
                 :boolean
                 (pop-item :string state))
      state)))

(define-registered
  file_EOF
  ^{:stack-types [:boolean]}
  (fn [state]
    (let [file (top-item :input state)
          result (empty? file)]
      (push-item result :boolean state))))

(define-registered
  file_begin
  ^{:stack-types [:input]}
  (fn [state]
    (push-item (stack-ref :input 1 state)
               :input
               (pop-item :input state))))

(define-registered
  output_charcount
  ^{:stack-types [:output]}
  (fn [state]
    (if (empty? (:integer state))
      state
      (let [top-int (top-item :integer state)]
        (stack-assoc top-int :input 2
                     (pop-item :integer state))))))

(define-registered
  output_wordcount
  ^{:stack-types [:output]}
  (fn [state]
    (if (empty? (:integer state))
      state
      (let [top-int (top-item :integer state)]
        (stack-assoc top-int :input 3
                     (pop-item :integer state))))))

(define-registered
  output_linecount
  ^{:stack-types [:output]}
  (fn [state]
    (if (empty? (:integer state))
      state
      (let [top-int (top-item :integer state)]
        (stack-assoc top-int :input 4
                     (pop-item :integer state))))))

; Atom generators
(def wc-atom-generators
  (concat
    (registered-for-stacks [:string :char :integer :boolean :exec])
    (list
      (fn [] (- (lrand-int 201) 100))
      (fn [] (lrand-nth ["\n" "\t" " "]))
      (fn [] (lrand-nth (concat ["\n" "\t"] (map (comp str char) (range 32 127)))))
      (tag-instruction-erc [:string :char :integer :boolean :exec] 1000)
      (tagged-instruction-erc 1000)
      ;;;; end ERCs
      'string_whitespace
      'file_readchar
      'file_readline
      'file_EOF
      'file_begin
      'output_charcount
      'output_wordcount
      'output_linecount
      ;;;; end problem-specific instructions
      )
    ))

;; Define test cases
(defn wc-input
  "Makes a WC input of length len."
  [len]
  (apply str
         (repeatedly len
                     (fn []
                       (let [rand-cond (lrand)]
                         (cond
                           (< rand-cond 0.05) \tab ;tab = 5%
                           (< rand-cond 0.1) \newline ;newline = 5%
                           (< rand-cond 0.35) \space ;space = 25%
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
   [(fn [] (str (wc-input (inc (lrand-int 99))) \newline)) 20 200] ;; Inputs ending in a newline
   [(fn [] (wc-input (inc (lrand-int 100)))) 158 1800] ;; Inputs that may or may not end in a newline
   ])

;;Can make WC test data like this:
;(test-and-train-data-from-domains wc-data-domains)

; Helper functions for error function
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
    (when true ;; Change to false to not print test cases
      (doseq [[i [string ch wo li]] (map vector (range) train-cases)]
        (println (str "Train Case " i " | Chars: " ch " Words: " wo " Lines: " li))
        (prn string)
        (println "--------------------"))
      (doseq [[i [string ch wo li]] (map vector (range) test-cases)]
        (println (str "Test Case " i " | Chars: " ch " Words: " wo " Lines: " li))
        (prn string)
        (println "--------------------")))
    (fn the-actual-wc-error-function
      ([program]
        (the-actual-wc-error-function program :train))
      ([program data-cases] ;; data-cases should be :train or :test
        (the-actual-wc-error-function program data-cases false))
      ([program data-cases print-outputs]
        (let [behavior (atom '())
              errors (flatten
                       (doall
                         (for [[input out-char out-word out-line] (case data-cases
                                                                    :train train-cases
                                                                    :test test-cases
                                                                    [])]
                           (let [final-state (run-push program
                                                       (->> (make-push-state)
                                                         (push-item nil :input)
                                                         (push-item nil :input)
                                                         (push-item nil :input)
                                                         (push-item input :input)
                                                         (push-item input :input)))
                                 result-char (stack-ref :input 2 final-state)
                                 result-word (stack-ref :input 3 final-state)
                                 result-line (stack-ref :input 4 final-state)]
                             (when print-outputs
                               (println (format "\nCorrect outputs | char: %d | word: %d | line: %d" out-char out-word out-line))
                               (println (format "Program outputs | char: %s | word: %s | line: %s" (pr-str result-char) (pr-str result-word) (pr-str result-line))))
                             ; Record the behavior
                             (when @global-print-behavioral-diversity
                               (swap! behavior concat [result-char result-word result-line]))
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
                                       100000))))))]
          (when @global-print-behavioral-diversity
            (swap! population-behaviors conj @behavior))
          errors)))))

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
      (doseq [[i error] (map vector
                             (range)
                             best-test-errors)]
        (println (format "Test Case  %3d | Error: %s" i (str error)))))
    (println ";;------------------------------")
    (println "Outputs of best individual on training cases:")
    (error-function best-program :train true)
    (println ";;******************************")
    )) ;; To do validation, could have this function return an altered best individual
       ;; with total-error > 0 if it had error of zero on train but not on validation
       ;; set. Would need a third category of data cases, or a defined split of training cases.

; Define the argmap
(def argmap
  {:error-function (wc-error-function wc-data-domains)
   :atom-generators wc-atom-generators
   :max-points 1000
   :max-points-in-initial-program 500
   :evalpush-limit 2000
   :population-size 1000
   :max-generations 300
   :parent-selection :lexicase
   :genetic-operator-probabilities {:alternation 0.2
                                    :uniform-mutation 0.2
                                    :uniform-close-mutation 0.1
                                    [:alternation :uniform-mutation] 0.5
                                    }
   :alternation-rate 0.01
   :alignment-deviation 10
   :uniform-mutation-rate 0.01
   :problem-specific-report wc-report
   :print-behavioral-diversity true
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 100000
   })
