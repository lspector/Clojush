(ns clojush.graphs.init.log
  (:require [plumbing.graph]
            [clojush.graphs.init.log.text :refer [text]]))

(def graph
  (plumbing.graph/graph
    text))
