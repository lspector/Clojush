(ns clojush.graphs.init
  (:require [plumbing.core :refer [defnk]]
            [plumbing.graph]
            [clj-random.core :as random]

            [clojush.graphs.init.log]
            [clojush.graphs.utils :refer [compile-graph]]))

(defnk problem-file [args]
  (first args))

(defnk param-list [args]
  (map #(if (.endsWith % ".ser")
           (str %)
           (read-string %))
       (rest args)))

(defnk example-params [problem-file]
  (require (symbol problem-file))
  (eval (symbol (str problem-file "/argmap"))))

(defnk args-str [args]
  (apply str (interpose \space args)))

(defnk params [example-params param-list]
  (merge example-params (apply sorted-map param-list)))

(defnk params-as-map [params]
  (into (sorted-map) params))

(defnk argmap-with-random-str [params-as-map]
  (if (:random-seed params-as-map)
    (update params-as-map :random-seed random/seed-to-string)
    params-as-map))

(def graph
  (plumbing.graph/graph
    args-str
    problem-file
    param-list
    example-params
    params
    params-as-map
    argmap-with-random-str
    :log clojush.graphs.init.log/graph))


(def ->init
  (compile-graph :init graph))
