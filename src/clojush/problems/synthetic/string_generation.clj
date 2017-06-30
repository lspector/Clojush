(ns clojush.problems.synthetic.string-generation
  (:use [clojush.pushgp.pushgp]
        [clojush util pushstate interpreter random])
  (:require [clojure.java.io :as io]))

#_"
The string generation problem asks for a mapping from an index to a character in a string.
The string is 15 characters long, containing only uppercase letters.
You can make the string dynamic by setting 'dynamic-string?' to true.
In this setting, the target string is dynamic, changing every 50 generations.
We calculate the difference in the number of zero error cases between the last generation of using a target string and the first generation of its use.
We keep a record of the differences in the 'improvement-vector' atom, which is displayed in every generation's problem-specific-report.
"

(def target-string-length 15)

(def dynamic-string? true)
(def new-string-every 50)
(def beginning-num-of-zero-cases (atom 0))
(def improvement-vector (atom []))

(def spit-improvement-vector? false)
(def output_folder "/home/juao15/autoconstruction/string-generation/improvement_logs")

(if (and spit-improvement-vector?
         (not (.exists (io/file output_folder))))
    (.mkdir (io/file output_folder)))

(def log-file
  (if spit-improvement-vector?
    (loop [i 0]
      (let [file (io/file output_folder (str i ".log"))]
        (if (.exists file)
          (recur (inc i))
          (do
            (spit file [])
            file))))))

(defn generate-target-string
  "Random string of uppercase letters"
  []
  (apply str (map char
                  (repeatedly target-string-length
                              #(+ 65 (rand-int 26))))))

(def target-string (atom (generate-target-string)))

(defn get-output-on-cases
  [program]
  (for [index (range target-string-length)]
    (let [state (run-push program 
                          (push-item index :input
                                     (push-item index :integer
                                                (make-push-state))))]
      (top-item :char state))))

(defn string-generation-error-fn
  [individual]
  (assoc individual
         :errors
         (mapv
          (fn [output target]
            (cond
              (= output :no-stack-item) 1000
              (= output target) 0
              :else 1))
          (get-output-on-cases (:program individual))
          @target-string)))

(defn report
  [best _ generation error-fn _]
  (print "\n******************************\n")
  (if dynamic-string?
    (let [effective-generation (mod generation new-string-every)]
      (if (zero? effective-generation)
        (reset! beginning-num-of-zero-cases (count (filter zero? (:errors (error-fn best))))))
      (if (= (dec new-string-every) effective-generation)
        (let [n-zero-cases (count (filter zero? (:errors (error-fn best))))]
          (swap! improvement-vector conj (- n-zero-cases @beginning-num-of-zero-cases))
          (reset! target-string (generate-target-string))
          (if spit-improvement-vector? (spit log-file @improvement-vector))
          (print "Replaced target string\n")))
      (print (str "Improvement vector: " @improvement-vector "\n"))))
  (print (str "Target string:         \"" @target-string "\"\n"))
  (print (str "Best string generated: \"" (apply str (get-output-on-cases (not-lazy (:program best)))) "\"\n"))
  (print "******************************\n\n"))

(def argmap
  {:error-function string-generation-error-fn
   :parent-selection :lexicase
   :max-points 500
   :atom-generators (cons (fn [] (+ 65 (int (lrand 26))))
                          (registered-for-stacks [:integer :boolean :char :exec]))
   :genetic-operator-probabilities {:alternation 0.2
                                    :uniform-mutation 0.2
                                    :uniform-close-mutation 0.1
                                    [:alternation :uniform-mutation] 0.5}
   :alternation-rate 0.05
   :alignment-deviation 10
   :uniform-mutation-rate 0.05
   :uniform-mutation-constant-tweak-rate 0.95
   :max-generations 100000
   :problem-specific-report report
   :exit-on-success false})
