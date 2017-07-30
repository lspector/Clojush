(ns clojush.log
  (:require [clojush.structured-logger :refer [->log!]]
            [clojush.graphs.events :refer [label->compute-graph]]
            [clojush.graphs.handlers :refer [name->handlers]]
            [clojush.utils :refer [wrap-graph]]))
;; this stuff is spread out over many files in subdirectories
;; Not only does this help with keeping files shorter, but it also allows
;; us to use the defnk names directly in the graph, because we don't
;; have to namespace them. For example, if we define `best` in the
;; lexicase file, then we can just throw that in the graph we defined
;; there, and it will registered under :best.

(def log!
  (->log!
    {:name->handler name->handlers
     :label->compute-graph label->compute-graph
     :wrap-graph wrap-graph}))
