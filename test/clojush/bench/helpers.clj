(ns clojush.bench.helpers
  "Tools to save samples for benchmarks and run them."
  (:require [clojure.java.io :as io]
            [clojush.pushgp.pushgp]
            [clj-random.core]
            [clojush.individual]
            [clojush.interpreter]
            [clojush.core]
            [clojure.walk]
            [clojush.pushstate]
            [clojush.pushgp.report])
  (:import (org.nustaq.serialization FSTObjectInput FSTObjectOutput FSTConfiguration)
           (java.util UUID)
           (java.io File FileOutputStream FileInputStream)
           (org.uncommons.maths.random RepeatableRNG)))

(def sampled-functions
  [{:fn-var #'clojush.interpreter/eval-push
    :save-prob (/ 1 10000000)}


   {:fn-var #'clojush.pushgp.pushgp/process-generation
    :save-prob (/ 1 50)
    ; we want to serialize the *values* inside the agents and the RNG
    ; instead of the objects themselves
    :serialize-inputs
    (fn [rand-gens pop-agents child-agents generation novelty-archive]
      [(into-array (map (fn [^RepeatableRNG rng] (.getSeed rng)) rand-gens))
       (into-array (map deref pop-agents))
       (into-array (map deref child-agents))
       generation
       novelty-archive])
    ;; then, before each execution, we want to deserialize them,
    ;; so that each execution get's its own new version
    :deserialize-inputs
    (fn [rand-gens pop-agents child-agents generation novelty-archive]
      [(vec (map clj-random.core/make-mersennetwister-rng rand-gens))
       (mapv agent pop-agents)
       (vec (map agent child-agents))
       generation
       novelty-archive])}])

; The args are taken from a configuration that performed well for Lee Spector
; https://push-language.hampshire.edu/t/improving-profiling-clojush-performance/904/19
(def call-main
  (partial clojush.core/-main
    "clojush.problems.software.replace-space-with-newline"
    ":autoconstructive" "true"
    ":autoconstructive-genome-instructions" ":all"
    ":autoconstructive-diversification-test" "[:not-a-clone :doesnt-clone]"
    ":report-simplifications" "0"
    ":meta-error-categories" "[:case-stagnation :autoconstruction-blindness]"
    ":print-history" "true"
    ":parent-selection" ":leaky-lexicase"
    ":lexicase-leakage" "0.1"
    ":max-generations" "5000"))

; all records that might be serialized should have be added here. Otherwise fast-serialization will fail
; in deserializing them.
(def serialize-classes
  [(class (clojush.individual/make-individual))
   clojush.pushstate.PushState])


(defn fn-str [fn-var]
  (str (:ns (meta fn-var))
       "__"
       (:name (meta fn-var))))

(defn sample-dir [fn-var]
  (io/file
    "bench-inputs"
    (fn-str fn-var)))

(defn sample-file [fn-var id]
  (io/file
    (sample-dir fn-var)
    (str id)))

(defmacro time-labeled
  [label & body]
  `(let [s# (new java.io.StringWriter)]
     (binding [*out* s#]
       (let [r# (time ~@body)]
         (binding [*out* *err*]
           (println ~label (str s#)))
         r#))))


(def ^FSTConfiguration conf (FSTConfiguration/createDefaultConfiguration))
(.registerClass conf (into-array Class serialize-classes))

;  from https://gist.github.com/orendon/e38ac86dcd4c64cadad8fd5c749452b7
;  but using fast-serialization.
(defn serialize-obj [object ^File file]
  (with-open [fos (FileOutputStream. file)]
    (with-open [outp (FSTObjectOutput. fos conf)]
      (time-labeled (str "Wrote " (.getPath file))
                    (.writeObject outp object)))))


(defn deserialize-obj [^File  file]
  (with-open [fis (FileInputStream. file)]
    (with-open [inp (FSTObjectInput. fis conf)]
      (time-labeled (str "Read " (.getPath file))
                    (.readObject inp)))))

;; use something like this if need to convert all the saved files to a new format
;(defn convert []
;  (doseq [{:keys [fn-var]} sampled-functions]
;    (println "doing fn" fn-var)
;    (doseq [name (.list (sample-dir fn-var))]
;      (when (not (clojure.string/ends-with? name "new"))
;        (println "doing" name)
;        (let [new-f (sample-file fn-var (str name "-new"))]
;          (when (not (.exists new-f))
;            (println "saving")
;            (serialize-obj (deserialize-obj-old (sample-file fn-var name)) new-f)))))))



(def generation-i (atom 0))

(defn save-sample
  [fn-symbol inputs]
  (let [f (sample-file fn-symbol (str @generation-i "-" (UUID/randomUUID)))]
    (io/make-parents f)
    (serialize-obj inputs f)))

(defn ->sample-fn [{:keys [fn-var save-prob serialize-inputs]}]
  (let [fn-original (var-get fn-var)
        serialize-inputs-fn (comp
                             (partial clojure.walk/postwalk identity)
                             (if serialize-inputs
                               (partial apply serialize-inputs)
                               identity))]
   [fn-var
    (fn [& inputs]
      (when (= fn-var #'clojush.pushgp.pushgp/process-generation)
        (reset! generation-i (nth inputs 3)))
      (when (< (rand) save-prob)
        (save-sample fn-var (serialize-inputs-fn inputs)))
      (apply fn-original inputs))]))

(defn sample []
  (as-> sampled-functions v
    (map ->sample-fn v)
    (into {} v)
    (with-redefs-fn v call-main)))

(defn setup-globals []
  (with-redefs-fn {#'clojush.pushgp.report/initial-report #(throw (Exception.))}
    #(time-labeled "Setup globals"
       (try
         (with-out-str (call-main))
         (catch Exception _  nil)))))

(def ->input (comp (fn [x] (setup-globals) x) deserialize-obj sample-file))


(def ->eval-push-input (partial ->input #'clojush.interpreter/eval-push))
(def ->process-generation-input (partial ->input #'clojush.pushgp.pushgp/process-generation))


(def f (:deserialize-inputs (nth sampled-functions 1)))
(defn process-generation-deserialize [& xs]
  (let [inputs (time-labeled "Setup inputs" (apply f xs))]
    (with-out-str
      (apply clojush.pushgp.pushgp/process-generation inputs))))
