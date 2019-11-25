(ns clojush.pushgp.selection.eliteness
  (:use [clojush random]))

; Note: eliteness is used by eliteness-based-tournament-selection below, but
; also by eliteness-based-batch-lexicase

(defn assign-eliteness-to-individual
  "Given an individual and the minimum error of all individuals on each case,
  assigns the :eliteness vector and :weighted-error"
  [ind min-error-on-each-case]
  (let [eliteness (map #(if (= %1 %2) 0 1)
                       min-error-on-each-case
                       (:errors ind))]
    (assoc ind :eliteness eliteness
               :weighted-error (apply + eliteness))))

(defn calculate-eliteness
  "Calculates whether each individual is elite or not on every
  error value. For each error, sets value in :elite-vector key to
  0 if elite and 1 if not elite. Also sets :weighted-error key, which
  can be used in eliteness-based-tournaments."
  [pop-agents {:keys [use-single-thread] :as argmap}]
  (let [pop (map deref pop-agents)
        min-error-on-each-case (map (partial apply min)
                                    (apply mapv vector ; transpose error vectors into vector of errors per case
                                           (map :errors pop)))]
    (dorun
     (map #((if use-single-thread swap! send)
            %
            assign-eliteness-to-individual
            min-error-on-each-case)
          pop-agents))
    (when-not use-single-thread (apply await pop-agents))))
