(ns clojush.graphs.generation.log
  (:require [plumbing.graph]

            [clojush.graphs.generation.log.text :refer [text]]
            [clojush.graphs.generation.log.edn :refer [edn]]
            [clojush.graphs.generation.log.csv :refer [csv]]
            [clojush.graphs.generation.log.json :refer [json]]
            [clojush.graphs.generation.log.remote :refer [remote]]))

(def graph
  (plumbing.graph/graph
    text
    edn
    csv
    json
    remote))
