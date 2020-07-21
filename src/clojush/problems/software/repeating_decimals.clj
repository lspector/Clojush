;; repeating_decimals.clj
;; Tom Helmuth, thelmuth@hamilton.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Count the number of repeating digits in a fraction
;; (given as a decimal representation). Input is a positive
;; integer n, and want to find the number of digits
;; that repeat in 1/n. For example, when n = 4,
;; 1/n = 0.250000, so 0 is the only repeated digit.
;; Another example, when n = 7,
;; 1/n = 0.14285714285714285, so 142857 are the repeated
;; digits and the answer should be 6.
;; 
;; 
;; SHOULD THIS be changed to have the answer
;; be the repeating decimal itself?
;; 

(ns clojush.problems.software.repeating-decimals
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag))

; Atom generators
(def atom-generators
  (concat (list
           1
           1.0
           ;;; end constants
           (fn [] (- (lrand-int 2001) 1000)) ;Integer ERC
           ;;; end ERCs
           (tag-instruction-erc [:integer :boolean :vector_integer :exec] 1000)
           (tagged-instruction-erc 1000)
            ;;; end tag ERCs
           'in1
            ;;; end input instructions
           )
          (registered-for-stacks [:integer :float :string :char :boolean :exec])))


(defn find-repeating-decimal
  "Uses long division to find repeating decimal 1/n.
   Returns the repeating decimal itself as a string"
  [n]
  (loop [curr-numerator 1
         decimals []
         seen-numerators []
                          corresponding-decimals []]
    (cond
      ; If zero, it just repeats zeros
    ;   (zero? curr-numerator) "0"
      ; If we've seen this numerator before, we're done and can find numerator
      (some #{curr-numerator} seen-numerators)
      (let [index (.indexOf seen-numerators curr-numerator)
            length-intro (count (nth corresponding-decimals index))]
        (apply str (drop length-intro decimals)))

      ; If curr-numerator is < n, need to multiply numerator by 10
      ; Need to add a 0 to decimals only if still < n after adding that 0
      (< (* 10 curr-numerator) n) (recur (* curr-numerator 10)
                                         (conj decimals 0)
                                         (conj seen-numerators curr-numerator)
                                         (conj corresponding-decimals decimals))
      ; If curr-numerator < n but curr-numerator * 10 >= n,
      ; don't add a 0 to decimals
      (< curr-numerator n) (recur (* curr-numerator 10)
                                  decimals
                                  seen-numerators
                                  corresponding-decimals)

      ; If curr-numerator is >= n, divide and recur
      :else (recur (mod curr-numerator n)
                   (conj decimals (int (/ curr-numerator n)))
                   (conj seen-numerators curr-numerator)
                   (conj corresponding-decimals decimals)))))


(comment

  (str 1 2 [1 2])
  
  (+ 2 (* 3 2))
  
  (sort [1 9 3 2 0 92])
  
  
  )


(find-repeating-decimal 773)

;; 773 => "0012936610608020698576972833117723156532988357050452781371280724450194049159120310478654592496765847347994825355756791720569210866752910737386804657179818887451487710219922380336351875808538163"
;; 76 => "315789473684210526"

(map find-repeating-decimal (range 1 20))
;; => ("0"
;;     "0"
;;     "3"
;;     "0"
;;     "0"
;;     "6"
;;     "142857"
;;     "0"
;;     "1"
;;     "0"
;;     "09"
;;     "3"
;;     "076923"
;;     "714285"
;;     "6"
;;     "0"
;;     "0588235294117647"
;;     "5"
;;     "052631578947368421")

(apply max-key #(count (second %)) (map #(vector % (find-repeating-decimal %)) (range 1 1000)))
;; => [983
;;     "0010172939979654120040691759918616480162767039674465920651068158697863682604272634791454730417090539165818921668362156663275686673448626653102746693794506612410986775178026449643947100712105798575788402848423194303153611393692777212614445574771108850457782299084435401831129196337741607324516785350966429298067141403865717192268565615462868769074262461851475076297049847405900305188199389623601220752797558494404883011190233977619532044760935910478128179043743641912512716174974567650050864699898270600203458799593082400813835198372329603255340793489318413021363173957273652085452695829094608341810783316378433367243133265513733468972533062054933875890132248219735503560528992878942014242115971515768056968463886063072227873855544252288911495422177009155645981688708036622583926754832146490335707019328585961342828077314343845371312309257375381485249237029501525940996948118006103763987792472024415055951169888097660223804679552390640895218718209562563580874872838250254323499491353"]

(count "0010172939979654120040691759918616480162767039674465920651068158697863682604272634791454730417090539165818921668362156663275686673448626653102746693794506612410986775178026449643947100712105798575788402848423194303153611393692777212614445574771108850457782299084435401831129196337741607324516785350966429298067141403865717192268565615462868769074262461851475076297049847405900305188199389623601220752797558494404883011190233977619532044760935910478128179043743641912512716174974567650050864699898270600203458799593082400813835198372329603255340793489318413021363173957273652085452695829094608341810783316378433367243133265513733468972533062054933875890132248219735503560528992878942014242115971515768056968463886063072227873855544252288911495422177009155645981688708036622583926754832146490335707019328585961342828077314343845371312309257375381485249237029501525940996948118006103763987792472024415055951169888097660223804679552390640895218718209562563580874872838250254323499491353")




(double (/ 1 6))
;; => 0.01315789473684211


;; Define test cases
; (defn mirror-image-input
;   "Makes a Mirror Image input vector of length len."
;   [len]
;   (vec (repeatedly len
;                    #(- (lrand-int 2001) 1000))))

; (defn change-a-few-elements
;   "Takes a vector and changes from 1 to 5 elements in the vector."
;   ([in-vec]
;    (change-a-few-elements in-vec (inc (lrand-int 5))))
;   ([in-vec num-to-change]
;    (if (>= 0 num-to-change)
;      in-vec
;      (change-a-few-elements (assoc in-vec (lrand-int (count in-vec)) (- (lrand-int 2001) 1000))
;                             (dec num-to-change)))))

; ;; A list of data domains for the problem. Each domain is a vector containing
; ;; a "set" of inputs and two integers representing how many cases from the set
; ;; should be used as training and testing cases respectively. Each "set" of
; ;; inputs is either a list or a function that, when called, will create a
; ;; random element of the set.
; (def mirror-image-data-domains
;   [[(list [[] []]) 1 0] ;; Empty vectors
;    [(list [[1] [1]]
;           [[1] [0]]
;           [[0] [1]]
;           [[16] [-44]]
;           [[-12] [-13]]) 5 0] ;; Length 1 vectors
;    [(list [[1 2] [2 1]]
;           [[1 1] [0 1]]
;           [[7 0] [0 7]]
;           [[5 8] [5 8]]
;           [[34 12] [34 12]]
;           [[456 456] [456 456]]
;           [[-431 -680] [40 831]]) 7 0] ;; Length 2 vectors
;    [(list [[1 2 1] [1 2 1]]
;           [[1 2 3 4 5 4 3 2 1] [1 2 3 4 5 4 3 2 1]]
;           [[45 99 0 12 44 7 7 44 12 0 99 45] [45 99 0 12 44 7 7 44 12 0 99 45]]
;           [(vec (concat (reverse (range 25)) (range 25))) (vec (concat (reverse (range 25)) (range 25)))]) 4 0] ;; Equal Palindromes
;    [(map #(vector [33 45 -941] (vec %))
;          (permutations [33 45 -941])) 6 0] ;; Permutations of a 3 item vector
;    [(fn [] (let [inA (mirror-image-input (inc (lrand-int 50)))]
;              (vector inA (vec (reverse inA))))) 37 500] ;; true cases
;    [(fn [] (let [inA (mirror-image-input (inc (lrand-int 50)))]
;              (vector inA inA))) 10 100] ;; equal vector cases
;    [(fn [] (let [inA (mirror-image-input (inc (lrand-int 50)))]
;              (vector inA (change-a-few-elements (vec (reverse inA)))))) 20 200] ;; close calls cases (change a few elements at most)
;    [(fn [] (let [inA (mirror-image-input (inc (lrand-int 50)))]
;              (vector inA (mirror-image-input (count inA))))) 10 200] ;; totally random cases
;    ])

; ;;Can make Mirror Image test data like this:
; ;(test-and-train-data-from-domains mirror-image-data-domains)

; ; Helper function for error function
; (defn mirror-image-test-cases
;   "Takes a sequence of inputs and gives IO test cases of the form
;    [input output]."
;   [inputs]
;   (map #(vector %
;                 (= (first %) (vec (reverse (second %)))))
;        inputs))

; (defn get-mirror-image-train-and-test
;   "Returns the train and test cases."
;   [data-domains]
;   (map mirror-image-test-cases
;        (test-and-train-data-from-domains data-domains)))

; ; Define train and test cases
; (def mirror-image-train-and-test-cases
;   (get-mirror-image-train-and-test mirror-image-data-domains))

; (defn mirror-image-evaluate-program-for-behaviors
;   "Evaluates the program on the given list of cases.
;    Returns the behaviors, a list of the outputs of the program on the inputs."
;   [program cases]
;   (doall
;    (for [[[input1 input2] output] cases]
;      (let [final-state (run-push program
;                                  (->> (make-push-state)
;                                       (push-item input2 :input)
;                                       (push-item input1 :input)))]
;        (top-item :boolean final-state)))))

; (defn mirror-image-errors-from-behaviors
;   "Takes a list of behaviors across the list of cases and finds the error
;    for each of those behaviors, returning an error vector."
;   [behaviors cases]
;   (map (fn [result correct-output]
;          (if (= result correct-output)
;            0
;            1))
;        behaviors
;        (map second cases)))

; (defn mirror-image-error-function
;   "The error function. Takes an individual as input,
;    and returns that individual with :errors and :behaviors set."
;   ([individual]
;    (mirror-image-error-function individual :train))
;   ([individual data-cases] ;; data-cases should be :train or :test
;    (let [cases (case data-cases
;                  :train (first mirror-image-train-and-test-cases)
;                  :test (second mirror-image-train-and-test-cases)
;                  data-cases)
;          behaviors (mirror-image-evaluate-program-for-behaviors (:program individual)
;                                                                 cases)
;          errors (mirror-image-errors-from-behaviors behaviors cases)]
;      (if (= data-cases :test)
;        (assoc individual :test-errors errors)
;        (assoc individual :behaviors behaviors :errors errors)))))

; (defn mirror-image-initial-report
;   [argmap]
;   (println "Train and test cases:")
;   (doseq [[i case] (map vector (range) (first mirror-image-train-and-test-cases))]
;     (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
;   (doseq [[i case] (map vector (range) (second mirror-image-train-and-test-cases))]
;     (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
;   (println ";;******************************"))

; (defn mirror-image-report
;   "Custom generational report."
;   [best population generation error-function report-simplifications]
;   (let [best-with-test (error-function best :test)
;         best-test-errors (:test-errors best-with-test)
;         best-total-test-error (apply +' best-test-errors)]
;     (println ";;******************************")
;     (printf ";; -*- Mirror Image problem report - generation %s\n" generation) (flush)
;     (println "Test total error for best:" best-total-test-error)
;     (println (format "Test mean error for best: %.5f" (double (/ best-total-test-error (count best-test-errors)))))
;     (when (zero? (:total-error best))
;       (doseq [[i error] (map vector
;                              (range)
;                              best-test-errors)]
;         (println (format "Test Case  %3d | Error: %s" i (str error)))))
;     (println ";;------------------------------")
;     (println "Outputs of best individual on training cases:")
;     (doseq [[correct-output result] (map vector
;                                          (map second (first mirror-image-train-and-test-cases))
;                                          (:behaviors best))]
;       (println (format "Correct output: %5b | Program output: %s" correct-output (str result))))
;     (println ";;******************************"))) ;; To do validation, could have this function return an altered best individual
;        ;; with total-error > 0 if it had error of zero on train but not on validation
;        ;; set. Would need a third category of data cases, or a defined split of training cases.


; ; Define the argmap
; (def argmap
;   {:error-function mirror-image-error-function
;    :training-cases (first mirror-image-train-and-test-cases)
;    :sub-training-cases '()
;    :atom-generators mirror-image-atom-generators
;    :max-points 1200
;    :max-genome-size-in-initial-program 150
;    :evalpush-limit 600
;    :population-size 1000
;    :max-generations 300
;    :parent-selection :lexicase
;    :genetic-operator-probabilities {:alternation 0.2
;                                     :uniform-mutation 0.2
;                                     :uniform-close-mutation 0.1
;                                     [:alternation :uniform-mutation] 0.5}
;    :alternation-rate 0.01
;    :alignment-deviation 10
;    :uniform-mutation-rate 0.01
;    :problem-specific-report mirror-image-report
;    :problem-specific-initial-report mirror-image-initial-report
;    :report-simplifications 0
;    :final-report-simplifications 5000
;    :max-error 1})
