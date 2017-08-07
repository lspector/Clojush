(ns clojush.graphs.generation.report
  (:require [plumbing.graph]
            [plumbing.core :refer [defnk map-vals fnk]]
            [cosmos.config]
            [clj-random.core :as random]

            [clojush.util :as util]
            [clojush.globals :as globals]
            [clojush.graphs.generation.report.lexicase]))


(defnk ifs-best [population]
  (apply min-key :weighted-error population))

(defnk err-fn [[:config [:argmap total-error-method]]]
  (if (= total-error-method :rmse) :weighted-error :total-error))

(defnk sorted [err-fn population [:config [:argmap total-error-method]]]
  (let [err-fn (if (= total-error-method :rmse) :weighted-error :total-error)]
    (sort-by err-fn < population)))

(defnk err-fn-best [sorted]
  (first sorted))

(defnk psr-best
  [[:config [:argmap error-function report-simplifications problem-specific-report]]
   err-fn-best
   population
   index]
  (problem-specific-report err-fn-best
                           population
                           index
                           error-function
                           report-simplifications))

(defnk best [psr-best err-fn-best]
  (if (:program psr-best)
    psr-best
    err-fn-best))


(defnk error-frequencies-by-case [population]
  (doall (map frequencies (apply map vector (map :errors population)))))


(defnk cosmos-data [population]
  (let [quants (cosmos.config/quantiles (count population))]
    (zipmap quants
            (map #(:total-error (nth (sort-by :total-error population) %))
                 quants))))

(defnk error-by-case-mean [population-errors]
  (apply map (fn [& args] (*' 1.0 (util/mean args)))
        population-errors))

(defnk error-by-case-min [population-errors]
  (apply map (fn [& args] (apply min args))
        population-errors))

(defnk population-meta-errors [population-raw]
  (doall (map :meta-errors population-raw)))

(defnk meta-error-by-category-mean [population-meta-errors]
   (apply map (fn [& args] (*' 1.0 (util/mean args)))
          population-meta-errors))

(defnk meta-error-by-category-min [population-meta-errors]
  (apply map (fn [& args] (apply min args))
        population-meta-errors))


(def stats-graph
  (plumbing.graph/graph
    :n (fnk [xs] (count xs))
    :min (fnk [xs] (apply min xs))
    :max (fnk [xs] (apply max xs))
    :mean (fnk [xs] (util/mean xs))
    :median (fnk [xs] (util/median xs))
    :sorted (fnk [xs] (sort xs))
    :first-quartile (fnk [sorted n] (nth sorted (util/truncate (/ n 4))))
    :third-quartile (fnk [sorted n] (nth sorted (util/truncate (/ (* 3 n) 4))))
    :standard-deviation
      (fnk [mean n xs]
        (Math/sqrt
          (/ (apply +' (map #(* (- % mean) (- % mean))
                            xs))
             (dec n))))))

(defn population-stats
  "Compute some summary statistics based on an attribute of every individual
  in the population. Got some attribute that doesn't exist yet? Add it to
  the individual graph first then you can query it here.

  Call this with the key of the attribute and put it in a submap in the graph"
  [attribute]
  (plumbing.graph/graph
    :xs (fnk [population]
          (doall (map attribute population)))
    stats-graph))

(defn diversity
  [attribute]
  (plumbing.graph/graph
    :frequencies-map (fnk [population]
                      (frequencies (map attribute population)))
    :frequency-stats
      (plumbing.graph/graph
        :xs (fnk [frequencies-map] (doall (vals frequencies-map)))
        stats-graph)
    :percent-unique
      (fnk [population frequencies-map [:config [:argmap population-size]]]
        (float (/ (count frequencies-map) population-size)))))

(defn sample-population-edit-distance
  "Returns a sample of Levenshtein distances between programs in the population,
   where each is divided by the length of the longer program."
  [pop samples]
  (let [instr-programs (map #(map :instruction %)
                            (map :genome pop))]
    (repeatedly samples
                #(let [prog1 (random/lrand-nth instr-programs)
                       prog2 (random/lrand-nth instr-programs)
                       longer-length (max (count prog1) (count prog2))]
                   (if (zero? longer-length)
                     0
                     (float (/ (util/levenshtein-distance prog1 prog2)
                               longer-length)))))))

(def homology-stats-graph
  (plumbing.graph/graph
    :xs (fnk [population]
            (doall (sample-population-edit-distance population 1000)))
    stats-graph))

(defnk selection-counts-sorted [[:config [:argmap population-size]]]
   (sort > (concat (vals @globals/selection-counts)
                   (repeat (- population-size (count @globals/selection-counts)) 0))))

(defnk reset-selection-counts! []
  (fn [] (reset! globals/selection-counts {})))

(defnk non-diversifying-n [population]
  (count (filter :is-random-replacement population)))

(defnk evaluations-count []
  globals/evaluations-count)

(defnk point-evaluations-before-report []
  @globals/point-evaluations-count)

(defnk reset-point-evaluations-count! [ point-evaluations-before-report]
  (fn [] (reset! globals/point-evaluations-count point-evaluations-before-report)))

(defnk timing-map [[:config timing-map]]
  @timing-map)

(defnk timing-map-total [timing-map]
  (apply + (vals timing-map)))

(defnk timing-map-total-seconds [timing-map-total]
  (float (/ timing-map-total 1000)))

(defnk timing-map-seconds [timing-map]
  (map-vals
    #(float (/ % 1000))
    timing-map))

(defnk timing-map-percent [timing-map timing-map-total]
  (map-vals
    #(* 100.0 (/ % timing-map-total))
    timing-map))

(defnk problem-specific-report-final-simplified-best
  [[:config [:argmap problem-specific-report
                     error-function
                     report-simplifications]]
   best
   index]
  (problem-specific-report (:final-simplification best) [] index error-function report-simplifications))


(defnk outcome
  [[:config [:argmap exit-on-success error-threshold max-generations max-point-evaluations]]
   best
   index]
  (cond (and exit-on-success
             (or (<= (:total-error best) error-threshold)
                 (:success best))) :success
        (>= index max-generations) :failure
        (>= @globals/point-evaluations-count max-point-evaluations) :failure
        :else :continue))


(def graph
  (plumbing.graph/graph
    ifs-best
    err-fn
    sorted
    err-fn-best
    psr-best
    best
    error-frequencies-by-case
    cosmos-data
    error-by-case-mean
    error-by-case-min
    population-meta-errors
    meta-error-by-category-mean
    meta-error-by-category-min
    :total-error-stats (population-stats :total-error)
    :genome-size-stats (population-stats :genome-size)
    :program-size-stats (population-stats :program-size)
    :program-percent-parens-stats (population-stats :program-percent-parens)
    :age-stats (population-stats :age)
    :grain-size-stats (population-stats :grain-size)
    :homology-stats homology-stats-graph
    :genome-diversity (diversity :genome)
    :program-diversity (diversity :program)
    :errors-diversity (diversity :errors)
    :total-error-diversity (diversity :total-error)
    :behaviors-diversity (diversity :behaviors)
    selection-counts-sorted
    reset-selection-counts!
    non-diversifying-n
    evaluations-count
    point-evaluations-before-report
    reset-point-evaluations-count!
    timing-map
    timing-map-total
    timing-map-total-seconds
    timing-map-seconds
    timing-map-percent
    problem-specific-report-final-simplified-best
    :lexicase clojush.graphs.generation.report.lexicase/graph
    outcome))
