(ns clojush.experimental.pushgp-map
  (:use [clojush.pushgp.pushgp]))

(defn pushgp-map
  "Calls pushgp with the args in argmap."
  [argmap]
  (apply pushgp (apply concat argmap)))
