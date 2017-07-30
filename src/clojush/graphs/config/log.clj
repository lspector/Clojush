(ns clojush.graphs.config.log
  (:require [plumbing.graph]
            [clojush.graphs.config.log.text :refer [text]]
            [clojush.graphs.config.log.edn :refer [edn]]))

(def graph
  (plumbing.graph/graph
    text
    edn))
