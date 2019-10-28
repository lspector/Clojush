(ns clojush.pushgp.selection.downsampled_lexicase
  (:use [clojush random]))

(defn down-sample
  "performs down-samplig on training cases by returning only a random sub-sample
   of the training cases"
  [{:keys [training-cases down-sample-factor]}]
  (take (* down-sample-factor (count training-cases))
        (shuffle training-cases)))

