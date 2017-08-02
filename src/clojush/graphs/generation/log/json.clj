(ns clojush.graphs.generation.log.json
  (:require [plumbing.core :refer [defnk]]
            [clojure.data.json :refer [json-str]]))

(defn jsonize-individual
  "Takes an individual and returns it with only the items of interest
   for the json logs."
  [log-fitnesses-for-all-cases json-log-program-strings index individual]
  (cond-> {:total-error (:total-error individual)
           :generation index
           :size (:program-size individual)}
    log-fitnesses-for-all-cases (assoc :errors (:errors individual))
    json-log-program-strings (assoc :program (:program-str individual))
    (:weighted-error individual) (assoc :weighted-error (:weighted-error individual))))


(defnk json!
  [[:config [:argmap json-log-filename
                     print-json-logs
                     log-fitnesses-for-all-cases
                     json-log-program-strings]]
   index
   population]
  (when print-json-logs
    (let [pop-json-string (json-str (map #(jsonize-individual
                                            log-fitnesses-for-all-cases
                                            json-log-program-strings
                                            index
                                            %)
                                         population))]
     (if (zero? index)
       (spit json-log-filename (str pop-json-string "\n") :append false)
       (spit json-log-filename (str "," pop-json-string "\n") :append true)))))
