(ns clojush.pushgp.selection.downsampled-lexicase
  (:use [clojush random]))

(defn down-sample
  "performs down-samplig on training cases by returning only a random sub-sample
   of the training cases"
  [{:keys [training-cases downsample-factor]}]
  (take (* downsample-factor (count training-cases))
        (shuffle training-cases)))

