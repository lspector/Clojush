(ns clojush.graphs.config.log
  (:require [plumbing.graph]
            [plumbing.core :refer [defnk]]

            [clojush.graphs.config.log.text :refer [text]]
            [clojush.graphs.config.log.edn :refer [edn]]))

(defnk all! [text edn])

(def graph
  (plumbing.graph/graph
    text
    edn
    all!))
