(ns clojush.pushgp.selection.selection
  (:use [clojush globals random]
        [clojush.pushgp.selection preselection tournament lexicase epsilon-lexicase
         elitegroup-lexicase random-threshold-lexicase random-toggle-lexicase 
         randomly-truncated-lexicase novelty rarified-lexicase subset-tournament
         fitness-proportionate]))

(defn select
  "Returns a selected parent."
  [pop {:keys [parent-selection print-selection-counts case-batch-size epsilon-lexicase-version] :as argmap}]
  (let [pop-with-meta-errors (map #(update-in % [:errors] (comp vec concat) (:meta-errors %)) pop)
        pop-with-meta-and-batch-errors (if (= case-batch-size 1)
                                         pop-with-meta-errors
                                         (batch-errors pop-with-meta-errors argmap))
        preselected (preselect pop-with-meta-and-batch-errors argmap)
        selected (case parent-selection
                   :tournament (tournament-selection preselected argmap)
                   :lexicase (lexicase-selection preselected argmap)
                   :downsampled-lexicase (lexicase-selection preselected argmap) ;; just uses lexicase; downsampling happens earlier
                   :epsilon-lexicase (case epsilon-lexicase-version
                                       ;; Semi-dynamic or dynamic
                                       (:semi-dynamic :dynamic)
                                       (epsilon-lexicase-selection preselected argmap)
                                       ;; Static epsilon lexicase
                                       :static
                                       (static-epsilon-lexicase-selection preselected argmap)
                                       ;; unrecognized version
                                       (throw (Exception. (str "Unrecognized argument for :epsilon-lexicase-version"
                                                               epsilon-lexicase-version))))
                   :elitegroup-lexicase (elitegroup-lexicase-selection preselected argmap)
                   :random-threshold-lexicase (random-threshold-lexicase-selection
                                               preselected argmap)
                   :random-toggle-lexicase (random-toggle-lexicase-selection
                                            preselected argmap)
                   :randomly-truncated-lexicase (randomly-truncated-lexicase-selection
                                                 preselected argmap)
                   :truncated-lexicase (truncated-lexicase-selection preselected argmap)
                   :leaky-lexicase (if (< (lrand) (:lexicase-leakage argmap))
                                     (lrand-nth preselected)
                                     (lexicase-selection preselected argmap))
                   :novelty-search (novelty-tournament-selection preselected argmap)
                   :uniform (lrand-nth preselected)
                   :rarified-lexicase (rarified-lexicase-selection preselected argmap)
                   :subset-tournament (subset-tournament-selection preselected argmap)
                   :fitness-proportionate (fitness-proportionate-selection preselected argmap)
                   (throw (Exception. (str "Unrecognized argument for parent-selection: "
                                           parent-selection))))]
    (when print-selection-counts
      (swap! selection-counts
             update-in
             [(:uuid selected)]
             (fn [sel-count] (if (nil? sel-count) 1 (inc sel-count)))))
    selected))

