(ns clojush.pushgp.selection.novelty
  (:use [clojush random]))

(defn select-individuals-for-novelty-archive
  ""
  [population argmap]
  (take (:individuals-for-novelty-archive-per-generation argmap)
        (lshuffle population)))

(defn calculate-novelty
  ""
  [pop-agents novelty-archive push-argmap]
  (println "^^^^^^^^^^ Novelty Archive below ^^^^^^^^^^^^^")
  (doseq [a novelty-archive]
    (println (:uuid a)))
  (println "vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv"))
