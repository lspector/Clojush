;; bowling.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;

(ns clojush.problems.software.benchmarks-v2.bowling
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower]
        ))

;; If the top item ion the string stack is a single character that is a bowling character,
;; return the equivalent integer. Otherwise, noop.
(define-registered
  string_bowling_atoi
  ^{:stack-types [:char :integer]}
  (fn [state]
    (if (empty? (:char state))
      state
      (let [top-char (stack-ref :char 0 state)]
        (if (not (some #{(first (str top-char))} "123456789-X/"))
          state
          (let [int-to-push (cond
                              (= \X top-char) 10
                              (= \/ top-char) 10
                              (= \- top-char) 0
                              true (Integer/parseInt (str top-char)))]
            (pop-item :char
                      (push-item int-to-push :integer state))))))))

; Atom generators
(def bowling-atom-generators
  (make-proportional-atom-generators
   (concat
    (registered-for-stacks [:integer :exec :string :char :boolean])
    (list (tag-instruction-erc [:integer :exec :boolean :string :char] 1000) ; tags
          (tagged-instruction-erc 1000)))
   (list 'in1) ; inputs
   (list \-
         \X
         \/
         \1
         \2
         \3
         \4
         \5
         \6
         \7
         \8
         \9
         10
         (fn [] (- (lrand-int 20) 10)) ; Integer ERC [-10, 10]
         ) ; constants
   {:proportion-inputs 0.15
    :proportion-constants 0.05}))

(defn convert-game
  "Takes a bowling input as a vector and converts it to a proper string"
  [vect-game]
  (loop [full-game (map str vect-game) ret-str "" frame-count 0]
    (cond
      (= full-game '()) ret-str ; The vector has been converted to a string
      (= frame-count 2) (recur full-game ret-str 0) ; Reset the count
      (= (first full-game) "10") (recur (rest full-game) (str ret-str "X") 0) ; Special case strike
      (and (> (count full-game) 1)
           (= (+ (Integer/parseInt (first full-game)) (Integer/parseInt (second full-game))) 10)
           (= frame-count 0))
              (recur (drop 2 full-game) (str ret-str (first full-game) "/") 0) ; Special case spare
      (= (first full-game) "0") (recur (rest full-game) (str ret-str "-") (inc frame-count)) ; Special case miss
      :else (recur (rest full-game) (str ret-str (first full-game)) (inc frame-count)))))

;; Define test cases
(defn bowling-input
  "Makes a bowling input."
  []
  (loop [frames 0 game []]
    (cond
      (>= frames 10) (convert-game game)   ; The game is generated, so return that
      (= frames 9) (let [score (lrand-int 11)   ; The last frame is very special
                         score2 (lrand-int (- 11 score))
                         score3 (lrand-int 11)
                         score4 (lrand-int 11)]
                         (cond
                            (= score 10) (recur (inc frames) (conj game score score3 score4))  ; Strike gets 2 more bowls
                            (= (+ score score2) 10) (recur (inc frames) (conj game score score2 score3)) ; Spare gets 1 more bowl
                            :else (recur (inc frames) (conj game score score2))))  ; Otherwise, just 2 normal bowls
      :else (let [score (lrand-int 11)    ; otherwise, generate 1 or two numbers
            score2 (lrand-int (- 11 score))]
           (if (= score 10)
               (recur (inc frames) (conj game score))
               (recur (inc frames) (conj game score score2)))))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def bowling-data-domains
  [[(list "--------------------" ; All gutter balls
          "XXXXXXXXXXXX" ; All strikes
          "5/5/5/5/5/5/5/5/5/5/5" ; All spares
          "7115XXX548/279-X53" ; Ending with a strike
          "532/4362X179-41447/5"  ; Ending with a spare
          "24815361356212813581"   ; No strikes, no spares
          "------X------------"  ; One strike, nothing else
          "----------3/--------" ; One spare, nothing else
          "--------------1-----"
          "11111111111111111111"
          "111111X111111111111"
          "-4-/-2-/-7-6-/-3-/-4"
          "-/-/-/-/-/-/-/-/-/-/-"
          "X52X52X52X52X52"
          "XXXXX----------"
          "XXXXX81XXX-1"
          "XXXX9/XXX2/XXX"
          "XXXXXXXXXXX9"
          "--X34--------------"
          "------3/61----------"
          "----------XX7-----" ; Other helpful edge cases
          ) 21 0]
   [(fn [] (bowling-input)) 179 2000]
  ])

;;Can make bowling test data like this:
;(test-and-train-data-from-domains bowling-data-domains)

; Converts a character into its proper score
(defn char-to-score
 [ch]
 (cond
   (= ch \X) 10
   (= ch \/) 10
   (= ch \-) 0
   true (Integer/parseInt (str ch))))

; Takes a bowling string and converts it to the score
(defn string-to-score
 [string frames]
 (if (zero? frames)
   0
   (let [frame-type (cond
                      (= (first string) \X) :strike
                      (= (second string) \/) :spare
                      true :neither)
         frame-total (case frame-type
                       :neither (+ (char-to-score (first string))
                                   (char-to-score (second string)))
                       :spare (+ 10 (char-to-score (get string 2)))
                       :strike (if (= (get string 2) \/)
                                 20
                                 (+ 10
                                    (char-to-score (second string))
                                    (char-to-score (get string 2)))))
         chars (if (= frame-type :strike)
                 1
                 2)]
     (+ frame-total
        (string-to-score (apply str (drop chars string))
                         (dec frames))))))

; Helper function for error function
(defn bowling-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (vector in
           (string-to-score in 10)))
       inputs))

(defn make-bowling-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-bowling-error-function
    ([individual]
      (the-actual-bowling-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-bowling-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            errors (doall
                     (for [[input1 correct-output] (case data-cases
                                                     :train train-cases
                                                     :test test-cases
                                                     [])]
                       (let [final-state (run-push (:program individual)
                                                   (->> (make-push-state)
                                                     (push-item input1 :input)))
                             result (stack-ref :integer 0 final-state)]
                         (when print-outputs
                           (println (format "Correct output: %6d | Program output: %s" correct-output (str result))))
                         ; Record the behavior
                         (swap! behavior conj result)
                         ; Error is integer distance
                         (if (number? result)
                           (abs (- result correct-output)) ;distance from correct integer
                           1000000) ;penalty for no return value
                         )))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

(defn get-bowling-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map sort (map bowling-test-cases
                 (test-and-train-data-from-domains data-domains))))

; Define train and test cases
(def bowling-train-and-test-cases
  (get-bowling-train-and-test bowling-data-domains))

(defn bowling-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first bowling-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second bowling-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn bowling-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Bowling problem report - generation %s\n" generation)(flush)
    (println "Test total error for best:" best-total-test-error)
    (println (format "Test mean error for best: %.5f" (double (/ best-total-test-error (count best-test-errors)))))
    (when (zero? (:total-error best))
      (doseq [[i error] (map vector
                             (range)
                             best-test-errors)]
        (println (format "Test Case  %3d | Error: %s" i (str error)))))
    (println ";;------------------------------")
    (println "Outputs of best individual on training cases:")
    (error-function best :train true)
    (println ";;******************************")
    )) ;; To do validation, could have this function return an altered best individual
       ;; with total-error > 0 if it had error of zero on train but not on validation
       ;; set. Would need a third category of data cases, or a defined split of training cases.


; Define the argmap
(def argmap
  {:error-function (make-bowling-error-function-from-cases (first bowling-train-and-test-cases)
                                                           (second bowling-train-and-test-cases))
   :atom-generators bowling-atom-generators
   :max-points 2000
   :max-genome-size-in-initial-program 250
   :evalpush-limit 2000
   :population-size 1000
   :max-generations 300
   :parent-selection :lexicase
   :genetic-operator-probabilities {:alternation 0.2
                                    :uniform-mutation 0.2
                                    :uniform-close-mutation 0.1
                                    [:alternation :uniform-mutation] 0.5}
   :alternation-rate 0.01
   :alignment-deviation 10
   :uniform-mutation-rate 0.01
   :problem-specific-report bowling-report
   :problem-specific-initial-report bowling-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000000})
