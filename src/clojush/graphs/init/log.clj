(ns clojush.graphs.init.log
  (:require [plumbing.graph]
            [plumbing.core :refer [defnk]]

            [clojush.graphs.init.log.text :refer [text]]))

(defnk all! [text])

(def graph
  (plumbing.graph/graph
    text
    all!))
