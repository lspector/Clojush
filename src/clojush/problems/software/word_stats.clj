;; word_stats.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given a file with <= 100 characters, print the number of words containing X
;; characters for X from 1 to the length of the longest word, in the format:
;;   words of length 1: 12
;;   words of length 2: 3
;;   ...
;; At the end of the output, should print a line that gives the number of
;; sentences, and line that gives the average number of words per sentence,
;; using the form:
;;  number of sentences: 4
;;  average sentence length: 7.452423455
;; A word is any string of consecutive non-whitespace characters (including
;; sentence terminators). Every input will contain at least one sentence
;; terminator (period, exclamation point, or question mark). The average number
;; of words should simply count the number of words in the file and divide by
;; the number of sentence terminator characters.
;;
;; input stack has the working file on top of stack, full file next

(ns clojush.problems.software.word-stats
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower)
    (:require [clojure.string :as string]))

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
          inp-result (push-item (if has-no-newline
                                  ""
                                  (subs file index))
                                :input
                                (pop-item :input state))]
      (if (= file "")
        state
        (if has-no-newline
          (push-item file :string inp-result)
          (push-item (subs file 0 index)
                     :string
                     inp-result))))))

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

; Atom generators
(def word-stats-atom-generators
  (concat (list
            \. \? \! \space \tab \newline
            []
            "words of length "
            ": "
            "number of sentences: "
            "average sentence length: "
            ;;; end constants
            (fn [] (- (lrand-int 201) 100)) ;Integer ERC
            ;;; end ERCs
            (tag-instruction-erc [:string :char :integer :boolean :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'file_readline
            'file_readchar
            'file_EOF
            'file_begin
            ;;; end input instructions
            )
          (registered-for-stacks [:string :vector_string :char :integer :vector_integer :float :vector_float :boolean :exec :print])))


;; Define test cases
(defn word-stats-input
  "Makes a Word Stats input of length len."
  [len]
  (let [candidate (apply str
                         (repeatedly len
                                     (fn []
                                       (let [rand-cond (lrand)
                                             tab-prob (+ 0.01 (* 0.02 (lrand))) ;prob of tab is between 0.01 and 0.03
                                             newline-prob (+ 0.02 (* 0.05 (lrand))) ;prob of newline is between 0.02 and 0.07
                                             space-prob (+ 0.05 (* 0.3 (lrand))) ;prob of space is between 0.05 and 0.35
                                             terminator-prob (+ 0.01 (* 0.19 (lrand))) ;prob of sentence terminator is between 0.01 and 0.20
                                             ]
                                         (cond
                                           (< rand-cond tab-prob) \tab
                                           (< rand-cond (+ tab-prob newline-prob)) \newline
                                           (< rand-cond (+ tab-prob newline-prob space-prob)) \space
                                           (< rand-cond (+ tab-prob newline-prob space-prob terminator-prob)) (lrand-nth [\. \! \?])
                                           :else (lrand-nth (map char (range 33 127))))))))]
    (if (some #{\. \! \?} candidate)
      candidate
      (apply str (assoc (vec candidate)
                        (lrand-int (count candidate))
                        (lrand-nth [\. \! \?]))))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def word-stats-data-domains
  [[(list ".", "!", "?", "\t.", "\n!", " ?", ".#", "A.\n", "! \n", "?\t\n", "\n?\n",
          ".!?.!?", ".txt", "!RACECAR!", "www.google.com"
          "Pirate basketball? Envelope WARS!"
          ".hello there wo.RLD"
          "out. at. the. plate."
          "nap time on planets!"
          "supercalifragilisticexpialidocious?"
          (apply str (concat (repeat 99 \newline) [\.]))
          (apply str (concat (repeat 99 \=) [\?]))
          (apply str (concat [\!] (repeat 99 \space)))
          (apply str (concat [\.] (repeat 99 \h)))
          (apply str (concat (repeat 99 \tab) [\?]))
          (apply str (concat (repeat 99 \@) [\!]))
          (apply str (repeat 100 \.))
          (apply str (repeat 100 \!))
          (apply str (repeat 100 \?))
          (apply str (take 100 (cycle (list \. \newline))))
          (apply str (take 100 (cycle (list \? \newline \newline))))
          (apply str (take 100 (cycle (list \! \D \newline))))
          (apply str (take 100 (cycle (list \! \space))))
          (apply str (take 100 (cycle (list \. \tab))))
          (apply str (take 100 (cycle (list \? \newline \y \space))))
          (apply str (take 100 (cycle (list \5 \!))))) 36 0] ;; "Special" inputs covering most base cases
   [(fn [] (word-stats-input (inc (lrand-int 100)))) 64 1000] ;; Random inputs
   ])

;;Can make Word Stats test data like this:
;(test-and-train-data-from-domains word-stats-data-domains)

; Helper function for error function
(defn word-stats-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (let [words (filter not-empty (string/split in #"\s+"))
               word-lengths (map count words)
               num-sentences (count (filter #(some #{%} ".?!") in))]
           (vector in
                   (vector (str (apply str (for [n (range 1 (inc (apply max word-lengths)))]
                                             (format "words of length %d: %d\n" n (count (filter #(= % n) word-lengths)))))
                                (format "number of sentences: %d\n" num-sentences)
                                (format "average sentence length: %s" (pr-str (round-to-n-decimal-places (* 1.0 (/ (count words) num-sentences)) 10))))
                           num-sentences
                           (float (/ (count words) num-sentences))))))
       inputs))

(defn make-word-stats-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-word-stats-error-function
    ([program]
      (the-actual-word-stats-error-function program :train))
    ([program data-cases] ;; data-cases should be :train or :test
                          (the-actual-word-stats-error-function program data-cases false))
    ([program data-cases print-outputs]
      (let [behavior (atom '())
            errors (flatten
                     (doall
                       (for [[input [correct-output sentences words-per-sentence]] (case data-cases
                                                                                     :train train-cases
                                                                                     :test test-cases
                                                                                     [])]
                         (let [final-state (run-push program
                                                     (->> (make-push-state)
                                                       (push-item input :input)
                                                       (push-item input :input)
                                                       (push-item "" :output)))
                               result (stack-ref :output 0 final-state)]
                           (when print-outputs
                             (println (format "\n| Correct output: %s\n| Program output: %s" (pr-str correct-output) (pr-str result))))
                           ; Record the behavior
                           (when @global-print-behavioral-diversity
                             (swap! behavior conj result))
                           ; Errors:
                           ;  1. Levenshtein distance of outputs
                           ;  2. If contains a line of the form #"number of sentences: (-?\d+)", then integer distance from correct output; otherwise penalty
                           ;  3. If contains a line of the form #"average sentence length: (-?\d+.\d+)", then float distance from correct output, rounded to 4 places; otherwise penalty
                           (vector
                             (levenshtein-distance correct-output result)
                             (if-let [result-n (try (Integer/parseInt (second (re-find #"number of sentences: (-?\d+)" result)))
                                                 (catch Exception e nil))]
                               (abs (- result-n sentences))
                               10000) ;Penalty
                             (if-let [result-f (try (Float/parseFloat (second (re-find #"average sentence length: (-?\d+\.\d+)" result)))
                                                 (catch Exception e nil))]
                               (round-to-n-decimal-places (abs (- result-f words-per-sentence)) 4)
                               10000.0) ;Penalty
                             )))))]
        (when @global-print-behavioral-diversity
          (swap! population-behaviors conj @behavior))
        errors))))

(defn get-word-stats-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map #(sort-by (comp count first) %)
       (map word-stats-test-cases
            (test-and-train-data-from-domains data-domains))))

; Define train and test cases
(def word-stats-train-and-test-cases
  (get-word-stats-train-and-test word-stats-data-domains))

(defn word-stats-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first word-stats-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second word-stats-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn word-stats-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-program (not-lazy (:program best))
        best-test-errors (error-function best-program :test)
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Word Stats problem report - generation %s\n" generation)(flush)
    (println "Test total error for best:" best-total-test-error)
    (println (format "Test mean error for best: %.5f" (double (/ best-total-test-error (count best-test-errors)))))
    (when (<= (:total-error best) 0.02)
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
  {:error-function (make-word-stats-error-function-from-cases (first word-stats-train-and-test-cases)
                                                              (second word-stats-train-and-test-cases))
   :atom-generators word-stats-atom-generators
   :max-points 3200
   :max-genome-size-in-initial-program 400
   :evalpush-limit 6000
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
   :problem-specific-report word-stats-report
   :problem-specific-initial-report word-stats-initial-report
   :print-behavioral-diversity true
   :report-simplifications 0
   :final-report-simplifications 5000
   :error-threshold 0.02
   :max-error 10000
   })
