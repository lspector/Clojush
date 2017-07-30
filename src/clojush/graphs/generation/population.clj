(ns clojush.graphs.generation.population
  (:require [plumbing.core :refer [defnk]]
            [plumbing.graph]

            [clojush.translate]
            [clojush.evaluate]
            [clojush.globals]
            [clojush.pushgp.selection.elitegroup-lexicase]
            [clojush.pushgp.selection.implicit-fitness-sharing]
            [clojush.pushgp.selection.novelty]
            [clojush.pushgp.selection.epsilon-lexicase]
            [clojush.experimental.decimation :refer [decimate]]
            [clojush.pushgp.breed :refer [breed]]
            [clojush.globals :as globals]))

(defnk translate-plush-to-push [index [:config record-time! argmap pop-agents]]
  (println "Processing generation:" index) (flush)
  (clojush.translate/population-translate-plush-to-push pop-agents argmap)
  (record-time! :reproduction))

(defnk compute-errors
  [translate-plush-to-push
   [:config rand-gens pop-agents record-time!
    [:argmap use-single-thread error-function :as argmap]]]
  (print "Computing errors... ") (flush)
  (let [f (if use-single-thread swap! send)]
    (dorun (map (fn [ind rand]
                  (f ind clojush.evaluate/evaluate-individual error-function rand argmap))
                pop-agents
                rand-gens)))
  (when-not use-single-thread (apply await pop-agents))
  (println "Done computing errors.") (flush)
  (record-time! :fitness))

(defnk calculate-hah-solution-rates [compute-errors [:config pop-agents argmap]]
  ;; calculate solution rates if necessary for historically-assessed hardness
  (clojush.evaluate/calculate-hah-solution-rates pop-agents argmap))

(defnk build-elitegroups [calculate-hah-solution-rates [:config pop-agents argmap]]
  ;; create global structure to support elite group lexicase selection
  (when (= (:parent-selection argmap) :elitegroup-lexicase)
    (clojush.pushgp.selection.elitegroup-lexicase/build-elitegroups pop-agents argmap)))

(defnk calculate-implicit-fitness-sharing [build-elitegroups [:config pop-agents argmap]]
  ;; calculate epsilons for epsilon lexicase selection
  (when (= (:total-error-method argmap) :ifs)
    (clojush.pushgp.selection.implicit-fitness-sharing/calculate-implicit-fitness-sharing pop-agents argmap)))

(defnk calculate-novelty [calculate-implicit-fitness-sharing [:config pop-agents argmap] novelty-archive]
  (when (or (= (:parent-selection argmap) :novelty-search)
            (some #{:novelty} (:meta-error-categories argmap)))
    (clojush.pushgp.selection.novelty/calculate-novelty pop-agents novelty-archive argmap)))


(defnk calculate-epsilons-for-epsilon-lexicase [calculate-novelty [:config pop-agents record-time! argmap] novelty-archive]
  (when (= (:parent-selection argmap) :epsilon-lexicase)
    (clojush.pushgp.selection.epsilon-lexicase/calculate-epsilons-for-epsilon-lexicase pop-agents argmap))
  (record-time! :other))


(defnk population-raw [calculate-epsilons-for-epsilon-lexicase [:config pop-agents argmap record-time!]]
  (doall (map deref pop-agents)))

(defnk next-novelty-archive [novelty-archive population-raw [:config argmap]]
  (concat novelty-archive
          (clojush.pushgp.selection.novelty/select-individuals-for-novelty-archive
            population-raw
            argmap)))

(defnk produce-new-offspring
  [[:config
    pop-agents
    child-agents
    rand-gens
    record-time!
    [:argmap
     decimation-ratio
     population-size
     decimation-tournament-size
     use-single-thread
     :as argmap]]]
  (record-time! :report)
  (println "\nProducing offspring...") (flush)
  (let [pop (if (>= decimation-ratio 1)
              (vec (doall (map deref pop-agents)))
              (decimate (vec (doall (map deref pop-agents)))
                        (int (* decimation-ratio population-size))
                        decimation-tournament-size))
        ages (map :age pop)]
    (reset! globals/min-age (apply min ages))
    (reset! globals/max-age (apply max ages))
    (dotimes [i population-size]
      ((if use-single-thread swap! send)
       (nth child-agents i)
       breed
       i (nth rand-gens i) pop argmap)))
  (when-not use-single-thread (apply await child-agents)))

(defnk install-next-generation
  [produce-new-offspring
   [:config pop-agents child-agents [:argmap population-size use-single-thread]]]
  (println "Installing next generation...") (flush)
  (dotimes [i population-size]
    ((if use-single-thread swap! send)
     (nth pop-agents i)
     (fn [av] (deref (nth child-agents i)))))
  (when-not use-single-thread (apply await pop-agents))) ;; SYNCHRONIZE

(def graph
  (plumbing.graph/graph
    translate-plush-to-push
    compute-errors
    calculate-hah-solution-rates
    build-elitegroups
    calculate-implicit-fitness-sharing
    calculate-novelty
    calculate-epsilons-for-epsilon-lexicase
    population-raw
    next-novelty-archive
    produce-new-offspring
    install-next-generation))
