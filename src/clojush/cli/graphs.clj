(ns clojush.cli.graphs
  (:require [puget.printer :as puget]
            [plumbing.fnk.pfnk :as pfnk]
            [plumbing.core :refer [map-vals]])
  (:import (schema.core.Predicate)
           (schema.core.AnythingSchema)))

(def schema-handlers
  {schema.core.Predicate (fn [_1 _2] nil)
   schema.core.AnythingSchema (fn [_1 _2] nil)})

(defn my-print [form]
  (puget/cprint
    form
    {:print-handlers schema-handlers}))

(defn symbol->value
  "Takes in a symbol like 'my.project/function and returns
  the value that the symbol refers to."
  [s]
  (let [namespace_ (namespace s)]
    ; (if namespace_
    (-> namespace_ symbol require))
      ; (throw {:type ::no-namespace :symbol s :hint "Couldn't evaluate this symbol, should be like `project/function`"})))
  (eval s))

(defn input [s]
  (my-print
    (pfnk/input-schema
      (symbol->value (symbol s)))))

(defn output [s]
  (my-print
    (pfnk/output-schema
      (symbol->value (symbol s)))))
