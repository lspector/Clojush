(ns clojush.experimental.pushgp_map
  (:use [clojush.clojush]))

(defn pushgp-map
  "Calls pushgp with the args in argmap."
  [argmap]
  (apply pushgp (apply concat argmap)))
