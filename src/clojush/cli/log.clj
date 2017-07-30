(ns clojush.cli.log
  (:require [puget.printer :as puget]
            [plumbing.fnk.pfnk :as pfnk]
            [plumbing.core :refer [map-vals]]

            [clojush.graphs.events :refer [label->compute-graph]]
            [clojush.graphs.events.generation.individual :as individual])
  (:import (schema.core.Predicate)
           (schema.core.AnythingSchema)))

(def schema-handlers
  {schema.core.Predicate (fn [_1 _2] nil)
   schema.core.AnythingSchema (fn [_1 _2] nil)})

(defn my-print [form]
  (puget/cprint
    form
    {:print-handlers schema-handlers}))


(defn event-inputs
  "Prints the mapping of each event to it's outputs"
  [& args]
  (my-print
    (map-vals pfnk/input-schema label->compute-graph)))

(defn event-outputs
  "Prints the mapping of each event to it's inputs"
  [& args]
  (my-print
    (map-vals pfnk/output-schema label->compute-graph)))


(defn individual-input [& args]
  (my-print
    (pfnk/input-schema
      individual/compute-graph)))

(defn individual-output [& args]
  (my-print
    (pfnk/output-schema
      individual/compute-graph)))
