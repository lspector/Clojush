(ns clojush.pushgp.selection.downsampled-lexicase
  (:use [clojush random]))

(defn down-sample
  "Performs downsampling on training cases by returning only a random subsample
   of the training cases"
  [{:keys [training-cases downsample-factor]}]
  (take (* downsample-factor (count training-cases))
        (shuffle training-cases)))
