;; this tests the full CLI process to make sure it outputs the same thing
;; it uses
(ns clojush.test.core-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [plumbing.core :refer [map-vals map-from-keys]]
            clojush.core
            clojush.args
            clojush.globals
            clojush.individual))

(defn ->cli-args [problem-file argmap]
  (let [argmap-strs (mapcat (partial map pr-str) argmap)]
    (conj argmap-strs (str problem-file))))

(defn censor-nondeterministic-output [out]
  (-> out
    (string/replace #"Clojush version = .*" "Clojush version = xxx")
    (string/replace #"Hash of last Git commit = .*" "Hash of last Git commit = xxx")
    (string/replace #"GitHub link = .*" "GitHub link = xxx")
    (string/replace #"Current time: 1\d+ milliseconds" "Current time: 1xxx milliseconds")
    (string/replace #"\d+.\d+ seconds" "x.x seconds")
    (string/replace #"\d+.\d+%" "x.x%")
    (string/replace #"log-filename = .*" "log-filename = xxx")
    (string/replace #"log-filename \"[^\"]*\"" "log-filename \"xxx\"")
    (string/replace #"#object\[[^]]+\]" "#object[xxx]")
    (string/replace "\r\n" "\n")))

(def globals
  [clojush.args/push-argmap
   clojush.globals/evaluations-count
   clojush.globals/point-evaluations-count
   clojush.globals/timer-atom
   clojush.globals/timing-map
   clojush.globals/solution-rates
   clojush.globals/elitegroups
   clojush.globals/elitegroups
   clojush.globals/epsilons-for-epsilon-lexicase
   clojush.globals/selection-counts
   clojush.globals/min-age
   clojush.globals/max-age])


(def initial-globals
  (for [global globals]
    [global @global]))

(defn reset-globals! []
  (doseq [[global intial-value] initial-globals]
    (reset! global intial-value)))

(defn format-tmp-file [format]
  (str "/tmp/clojush." format))

(defn format->tmp-filename [format]
  (str (java.io.File/createTempFile "clojush" (str "." format))))

(def universal-argmap
  {:use-single-thread true
   :max-generations 0
   :population-size 5
   :csv-log-filename (format->tmp-filename "csv")
   :edn-log-filename (format->tmp-filename "edn")
   :json-log-filename (format->tmp-filename "json")})

(def mock-uuid (java.util.UUID/fromString  "00000000-0000-0000-0000-000000000000"))

(def old-make-individual clojush.individual/make-individual)
(defn mocked-make-individual [& args]
  (assoc
    (apply old-make-individual args)
    :uuid
    mock-uuid))

(def old-load-push-argmap clojush.args/load-push-argmap)
(defn mocked-load-push-argmap [argmap]
  (old-load-push-argmap argmap)
  (swap! clojush.args/push-argmap assoc
    :run-uuid mock-uuid
    :random-seed (byte-array (repeat 16 0))))

(defn clojush-main [cli-args]
  (with-out-str
    (with-redefs [clojush.individual/make-individual mocked-make-individual
                  clojush.args/load-push-argmap mocked-load-push-argmap]
      (apply clojush.core/-main cli-args))))

(defn filename-for-format [argmap format]
  (-> format
    name
    (str "-log-filename")
    keyword
    argmap))

(defn problem->formats
  "Calls clojush and returns a mapping of format to output, like {:txt <whatevever> :csv <whatever>}"
  [problem-file argmap other-formats]
  (reset-globals!)
  (let [total-argmap (merge universal-argmap argmap)
        cli-args (->cli-args problem-file total-argmap)
        output (clojush-main cli-args)]
    (->> {"txt" output}
      (merge
        (map-from-keys
          (comp slurp (partial filename-for-format total-argmap))
          other-formats))
      (map-vals censor-nondeterministic-output))))

(def test-problems
  (->>
    [[:rswn-success
      {:problem-file 'clojush.problems.software.replace-space-with-newline
       :argmap {:error-threshold 100975
                :max-generations 20
                :population-size 5
                :maintain-ancestors true
                :print-ancestors-of-solution true}}]
     [:rswn-autoconstructive
      {:problem-file 'clojush.problems.software.replace-space-with-newline
       :argmap {:autoconstructive true}}]
     [:rswn-print-everything
      {:problem-file 'clojush.problems.software.replace-space-with-newline
       :argmap {:print-errors false
                :print-history true
                :print-timings true
                :print-error-frequencies-by-case true
                :maintain-ancestors true
                :print-homology-data true
                :print-cosmos-data true
                :print-csv-logs true
                :csv-columns
                  [:generation :location :parent-uuids :genetic-operators
                   :push-program-size :plush-genome-size :push-program
                   :plush-genome :total-error :test-case-errors]
                :print-edn-logs true
                :edn-additional-keys
                  [:generation :location :push-program-size :plush-genome-size]
                :print-json-logs true
                :log-fitnesses-for-all-cases true
                :json-log-program-strings true}
       :other-formats ["edn" "csv" "json"]}]
     [:nth-prime-ifs
      {:problem-file 'clojush.problems.integer-regression.nth-prime
       :argmap {:total-error-method :ifs
                :normalization :divide-by-max-error}}]]
    (into {})))
(defn ->path [label format]
  (str (io/file "test-outputs" (str (name label) "." format))))

(defn regenerate
  "Regenerates all test outputs.

  If any args are passed in, only will regenerate those labels."
  [& args]
  (let [input-labels (set (map keyword args))
        selected-labels (if (empty? input-labels) (keys test-problems) input-labels)
        selected-problems (select-keys test-problems selected-labels)]
    (println "Regenerating test outputs for" (keys selected-problems))
    (doseq [[label {:keys [problem-file argmap other-formats]}] selected-problems]
      (println "Running" label "->" problem-file argmap)
      (doseq [[format out] (problem->formats problem-file argmap other-formats)
              :let [path (->path label format)]]
        (println "Saving" path)
        (spit path out)))))

(defn execute-test [label]
  (let [{:keys [problem-file argmap other-formats]} (test-problems label)]
    (doseq [[format out] (problem->formats problem-file argmap other-formats)
            :let [path (->path label format)]]
      (is (= (slurp path)
             out)))))

(doseq [label (keys test-problems)]
  (eval
    `(deftest ~(symbol (name label))
       (execute-test ~label))))
