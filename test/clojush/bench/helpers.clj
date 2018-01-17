(ns clojush.bench.helpers
  (:require [clojure.java.io :as io]
            [clojush.core :refer [-main]]
            [clojush.pushgp.report :refer [initial-report]]))


(defn call-main [generations serial population-size]
  (with-out-str
    (-main
      "clojush.problems.software.replace-space-with-newline"
      ":autoconstructive" "true"
      ":autoconstructive-genome-instructions" ":all"
      ":autoconstructive-diversification-test" "[:not-a-clone :doesnt-clone]"
      ":report-simplifications" "0"
      ":meta-error-categories" "[:case-stagnation :autoconstruction-blindness]"
      ":print-history" "true"
      ":parent-selection" ":leaky-lexicase"
      ":lexicase-leakage" "0.1"
      ":max-generations" (str generations)
      ":use-single-thread" (str serial)
      ":population-size" (str population-size))))

(defn grab-call-inputs
  "Returns the first `n` inputs to the `func-symb`, from running
   the cli args.

  Does this by wrapping `func-symb` function to save each call, and
  raising an exception after we have enough calls done, which is then caught."
  [func-symb n-generations]
  (require (symbol (namespace func-symb)))
  (let [inputs (java.util.concurrent.ConcurrentLinkedQueue.)
        func-var (eval `(var ~func-symb))
        func-original (var-get func-var)
        func-log (fn [& input]
                   (.add inputs input)
                   (apply func-original input))]
    (with-redefs-fn {func-var func-log}
      (fn []
        (call-main n-generations false 200)))
    inputs))

(defn var->symb [v]
  (let [{:keys [ns name]} (meta v)]
    (symbol (str ns) (str name))))

;  from https://gist.github.com/orendon/e38ac86dcd4c64cadad8fd5c749452b7
(defn serialize-obj [object file]
  (with-open [outp (-> file java.io.FileOutputStream. org.nustaq.serialization.FSTObjectOutput.)]
    (.writeObject outp object)))

(defn deserialize-obj [file]
  (with-open [inp (-> file java.io.FileInputStream. org.nustaq.serialization.FSTObjectInput.)]
    (.readObject inp)))

(defn input-file [symb]
  (io/file
    "bench-inputs"
    (clojure.string/replace (str symb) #"/" "_")))

(defn load-symbol [s]
  (require (symbol (namespace s)))
  (let [v (eval `(var ~s))]
    (var-get v)))

(defn save-call-inputs
  "Saves all the calls to the function for benchmarking"
  [func-symb-str n-generations-str]
  (println "Grabbing inputs for" func-symb-str)
  (let [inputs (java.util.ArrayList. (grab-call-inputs (symbol func-symb-str) (Integer. n-generations-str)))
        file (input-file func-symb-str)]
    (println "Saving" (.size inputs) "inputs to" (str file))
    (serialize-obj inputs file)
    (println "Saved file")))

(defn set-globals! []
  (with-redefs [initial-report (fn [_] (throw (Exception. "")))]
    (try
      (call-main 0 true 200)
      (catch Exception _))))
(defn load-call-inputs [func-symb]
  (set-globals!)
  (deserialize-obj (input-file func-symb)))

(defn run-fn [bh fn- possible-inputs]
 (doseq [input possible-inputs]
   (.consume bh (apply fn- input))))
