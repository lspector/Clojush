(ns clojush.bench.helpers
  (:require [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.future :refer :all]
            [clojure.spec.test.alpha :as stest]

            [clojush.core :refer [-main]]
            [clojush.pushgp.pushgp :refer [process-generation]]))

(def call-main
  (partial -main
    "clojush.problems.software.replace-space-with-newline"
    ":autoconstructive" "true"
    ":autoconstructive-genome-instructions" ":all"
    ":autoconstructive-diversification-test" "[:not-a-clone :doesnt-clone]"
    ":report-simplifications" "0"
    ":meta-error-categories" "[:case-stagnation :autoconstruction-blindness]"
    ":print-history" "true"
    ":parent-selection" ":leaky-lexicase"
    ":lexicase-leakage" "0.1"))

(defn symb->var [s]
  (eval `(var ~s)))

(defn load-symbol [s]
  (require (symbol (namespace s)))
  (var-get (symb->var v)))

(s/fdef load-symbol
  :args (s/and :s symbol?)
  :ret (s/and :v var? :val any?))

(defn run-sample [configs-str]
  (sample (eval configs-str)))

(s/def ::fn-symbol symbol?)
(s/def ::save-prob (s/and number? #(<= 0 % 1)))
(s/def ::sample-config (s/keys :req-un [::fn-symbol ::save-prob]))
(s/def ::sample-configs (s/coll-of ::sample-config))

(defn sample
  [configs]
  (as-> v
    (map (fn [{:keys fn-symbol save-prob}]
           (let [fn-original (load-symbol fn-symbol)
                 fn-log (fn [& inputs]
                          (if (< rand save-prob)
                            (let [globals (get-globals)
                                  ret (apply fn-original inputs)]
                              (binding [*out* *err*]
                                (time (save-sample fn-symbol inputs globals))
                                (println fn-symbol)
                                (flush))
                              ret)
                            (apply fn-original inputs)))]))
         v)
    (into {} v)
    (with-redefs-fn v call-main)))

(s/fdef sample
  :args (s/and :configs ::sample-configs)
  :ret nil?)

(defn var->symb [v]
  (let [{:keys [ns name]} (meta v)]
    (symbol (str ns) (str name))))

(def atom-globals
  [#'clojush.globals/evaluations-count
   #'clojush.globals/point-evaluations-count
   #'clojush.globals/epsilons-for-epsilon-lexicase])

(def var-globals
  [#'clj-random.core/*RNG*])

(defn get-globals []
  (let [atoms (->> atom-globals
                   (map #([(var->symb %) @(var-get %)]))
                   (into {}))
        vars (->> var-globals
                  (map #([(var->symb %) (var-get %)]))
                  (into {}))]
    {:atoms atoms :vars vars}))

(s/def ::atoms (s/map-of symbol? any?))
(s/def ::vars (s/map-of symbol? any?))
(s/def ::globals (s/keys :req-un [::atoms ::vars]))
(s/fdef get-globals
  :ret ::globals)


(defn save-sample
  [fn-symbol inputs globals]
  (let [f (sample-file fn-symbol)]
    (clojure.java.io/make-parents f)
    (serialize-obj {:inputs inputs :globals globals}
                   f)))
(s/def ::inputs (s/coll-of any?))
(s/fdef save-sample
  :args (s/cat :fn-symbol ::fn-symbol
               :inputs ::inputs
               :globals ::globals))


(def data-dir
  (io/file
    "test"
    "clojush"
    "bench"
    "data"))

(defn fn-str [fn-symb]
  (clojure.string/replace (str fn-symb) #"/" "_"))

(defn sample-file-dir [fn-symb]
  (io/file data-dir "sample" (fn-str fn-symbol)))

(defn sample-file [fn-symb]
  (io/file
    (sample-file-dir fn-symb)
    (str (java.util.UUID/randomUUID))))

(defn selected-file [fn-symbol i]
  (io/file
    data-dir
    "selected"
    (fn-str fn-symbol)
    (str i)))

(s/def ::sample (s/keys :req-un [::inputs ::globals]))

;  from https://gist.github.com/orendon/e38ac86dcd4c64cadad8fd5c749452b7
(defn serialize-obj [object file]
  (with-open [outp (-> file java.io.FileOutputStream. org.nustaq.serialization.FSTObjectOutput.)]
    (.writeObject outp object)))

(s/fdef serialize-obj
  :args (s/cat :object ::sample :file any?))


(defn deserialize-obj [file]
  (with-open [inp (-> file java.io.FileInputStream. org.nustaq.serialization.FSTObjectInput.)]
    (.readObject inp)))


(s/fdef deserialize-obj
  :ret ::sample)



(def NUM-SAMPLES 10)

(defn select-samples [fn-symbol]
  (let [files
        (->> sample-file-dir
          file-seq
          shuffle
          (take NUM-SAMPLES)
          (map-indexed (fn [i f] [f (selected-file fn-symbol i)])))]
    (doseq [[sample-file selected-file] files]
      (clojure.java.io/make-parents selected-file)
      (clojure.java.io/copy sample-file selected-file))))


(defn ->sample [fn-symbol i]
  (->> (selected-file fn-symbol i)
    deserialize-obj))


(def atom-globals-vals {})
(def var-globals-vals {})

(defn set-globals! []
  (doseq [[global intial-value] atom-globals-vals]
    (reset! global intial-value))
  (doseq [[global intial-value] atom-globals-vals]
    (set! global intial-value)))



(defn load-sample [fn-symbol i])


(def load-eval-push-input (partial load-sample 'clojush.interpreter/eval-push))

(defn init-globals! []
  (with-redefs [process-generation (fn [_] (throw (Exception. "")))]
    (try
      (with-out-str (call-main))
      (catch Exception _))))

(defn set-globals! [globals])

(defn load-call-inputs [func-symb]
  (set-globals!)
  (deserialize-obj (input-file func-symb)))

(defn run-fn [bh fn- possible-inputs]
 (doseq [input possible-inputs]
   (.consume bh (apply fn- input))))
