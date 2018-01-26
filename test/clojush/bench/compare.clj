(ns clojush.bench.compare
  (:import [org.apache.commons.math3.distribution TDistribution])
  (:require [clojure.edn :as edn]
            [clojure.math.numeric-tower :as math]
            [jmh.task :refer [format-table]]
            [clojure.spec.alpha :as s]
            [clojure.future :refer :all]
            [clojure.spec.test.alpha :as stest]))

(def CI 0.95)

(defn- sq
  [x]
  (math/expt x 2))

(s/def ::mean number?)
(s/def ::variance number?)
(s/def ::n int?)
(s/def ::stats (s/keys :req-un [::mean ::variance ::n]))


(defn ratio
  "Returns the ratio of the of the new mean to the old mean
   with the 95% CI

  Uses the formula from the 'Quantifying Performance Changes with Effect Size Confidence Intervals'
  paper (6.2)
  which is the same as the formula from 'Rigorous Benchmarking in Reasonable Time'
  (5)"
  [{o-m :mean o-v :variance o-n :n}
   {n-m :mean n-v :variance n-n :n}]
  (assert (= o-n n-n))
  (let [n o-n
        dof (- n 1)
        alpha (/ (- 1 CI) 2)
        t (sq (.inverseCumulativeProbability
                (TDistribution. dof)
                (/ alpha 2)))
        denom (- (sq o-m)
                 (/ (* t o-v)
                    n))
        r (/ (* o-m n-m)
             denom)
        ci-num-in  (- (sq (* o-m n-m))
                      (* (- (sq o-m)
                            (/ (* t o-v)
                               n))
                         (- (sq n-m)
                            (/ (* t n-v)
                               n))))
        ci (/ (math/sqrt ci-num-in)
              denom)]
    (str (format "%.2f" r)
         " ± (95%) "
         (format "%.2f" ci))))

(s/fdef ratio
        :args (s/cat :old ::stats :new ::stats)
        :ret string?)

(stest/instrument `ratio)

(defn ->results [filename]
  (-> filename slurp edn/read-string))


(defn map-intersection
  "takes a sequence of maps and return a new sequence of maps with only the
   common keys remaining"
  [ms]
  (let [common-keys (->> ms
                         (map (comp set keys))
                         (apply clojure.set/intersection))]
    (map #(select-keys % common-keys) ms)))

(s/fdef map-intersection
        :args (s/cat :ms (s/coll-of (s/map-of any? any?)))
        :ret (s/coll-of (s/map-of any? any?)))


(stest/instrument `map-intersection)


(defn -main [old-filename new-filename]
  "lein run -m clojush.bench.compare <old filename> <new filename>

  Compares the benchmarks results saved in `old filename` to those in `new filename`.
  It prints out the ratio of the new to the old means, with a 95% CI for each benchmark
  in both files."
  (let [keys #{:name :params :samples :fn}]
    (->> [old-filename new-filename]
      ;; makes two maps of {keys -> {:mean ...}}
      (map (comp (partial into {})
              (partial map (juxt #(select-keys % keys) :statistics))
              ->results))
      ;; filters for only experiments run in both
      map-intersection
      ;; calculates mean ratio and CI of new/old
      (apply merge-with ratio)
      (map (fn [[ks r]] (assoc ks :mean-ratio r)))
      (format-table (conj keys :mean-ratio))
      println)))
