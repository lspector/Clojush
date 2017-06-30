(ns clojush.pushgp.selection.selection
  (:use [clojush globals random]
        [clojush.pushgp.selection preselection tournament lexicase epsilon-lexicase
         elitegroup-lexicase random-threshold-lexicase novelty]))

(defn select
  "Returns a selected parent."
  [pop {:keys [parent-selection print-selection-counts] :as argmap}]
  (let [pop-with-meta-errors (map #(update-in % [:errors] concat (:meta-errors %)) pop)
        preselected (preselect pop-with-meta-errors argmap)
        selected (case parent-selection
                   :tournament (tournament-selection preselected argmap)
                   :lexicase (lexicase-selection preselected argmap)
                   :epsilon-lexicase (epsilon-lexicase-selection preselected argmap)
                   :static-epsilon-lexicase (static-epsilon-lexicase-selection pop-with-meta-errors)
                   :super-dynamic-epsilon-lexicase (super-dynamic-epsilon-lexicase-selection pop-with-meta-errors location argmap)
                   :elitegroup-lexicase (elitegroup-lexicase-selection preselected argmap)
                   :random-threshold-lexicase (random-threshold-lexicase-selection 
                                                preselected argmap)
                   :leaky-lexicase (if (< (lrand) (:lexicase-leakage argmap))
                                     (lrand-nth preselected)
                                     (lexicase-selection preselected argmap))
                   :novelty-search (novelty-tournament-selection preselected argmap)
                   :uniform (lrand-nth preselected)
                   (throw (Exception. (str "Unrecognized argument for parent-selection: "
                                           parent-selection))))]
    (when print-selection-counts
      (swap! selection-counts 
             update-in 
             [(:uuid selected)] 
             (fn [sel-count] (if (nil? sel-count) 1 (inc sel-count)))))
    selected))

