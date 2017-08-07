(ns clojush.graphs.generation
  (:require [plumbing.graph :as graph]
            [plumbing.core :refer [defnk fnk map-vals]]
            [cosmos.config :as config]
            [clj-random.core :as random]

            [clojush.util :as util]
            [clojush.globals :as globals]
            [clojush.graphs.utils :refer [compile-graph]]
            [clojush.graphs.individual :refer [->individual]]
            [clojush.graphs.generation.log]
            [clojush.graphs.generation.population]
            [clojush.graphs.generation.report]))

(defnk population-errors [population-raw]
  (doall (map :errors population-raw)))

(defnk errors-n [population-raw]
  (-> population-raw first :errors count))

(defnk min-error-by-case [population-errors]
  (doall (apply map
                (fn [& args] (apply min args))
                population-errors)))

;; we want to dynamically calculate additional properties on each individual, so we only calculate things like
;; % perenthesis or mean error once for each individual, regardless of where we are using that information
(defnk population [population-raw min-error-by-case [:config argmap]]
  ;; include the argmap and min-error-by-case in each individual's graph
  ;; so that we can calculate the n-elite-cases
  (->> population-raw
    (map (fn [ind]
          (merge
            (->individual (assoc ind
                           :argmap argmap
                           :min-error-by-case min-error-by-case))
            ind)))
    doall))

(def graph
  (plumbing.graph/graph
    clojush.graphs.generation.population/graph
    errors-n
    population-errors
    min-error-by-case
    population
    :report clojush.graphs.generation.report/graph
    :log clojush.graphs.generation.log/graph))

(def ->generation
  (compile-graph :generation graph))
