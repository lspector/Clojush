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

(defnk plush->push! [index [:config argmap pop-agents]]
  (clojush.translate/population-translate-plush-to-push pop-agents argmap))

(defnk errors!
  [[:config rand-gens pop-agents
    [:argmap use-single-thread error-function :as argmap]]]
  (let [f (if use-single-thread swap! send)]
    (dorun (map (fn [ind rand]
                  (f ind clojush.evaluate/evaluate-individual error-function rand argmap))
                pop-agents
                rand-gens)))
  (when-not use-single-thread (apply await pop-agents)))


(defnk hah-solution-rates! [[:config pop-agents argmap]]
  ;; calculate solution rates if necessary for historically-assessed hardness
  (clojush.evaluate/calculate-hah-solution-rates pop-agents argmap))

(defnk elitegroups! [[:config pop-agents argmap]]
  ;; create global structure to support elite group lexicase selection
  (when (= (:parent-selection argmap) :elitegroup-lexicase)
    (clojush.pushgp.selection.elitegroup-lexicase/build-elitegroups pop-agents argmap)))

(defnk implicit-fitness-sharing! [[:config pop-agents argmap]]
  (when (= (:total-error-method argmap) :ifs)
    (clojush.pushgp.selection.implicit-fitness-sharing/calculate-implicit-fitness-sharing pop-agents argmap)))

(defnk novelty! [[:config pop-agents argmap] novelty-archive]
  (when (or (= (:parent-selection argmap) :novelty-search)
            (some #{:novelty} (:meta-error-categories argmap)))
    (clojush.pushgp.selection.novelty/calculate-novelty pop-agents novelty-archive argmap)))


(defnk epsilons-for-epsilon-lexicase! [[:config pop-agents argmap] novelty-archive]
  ;; calculate epsilons for epsilon lexicase selection
  (when (= (:parent-selection argmap) :epsilon-lexicase)
    (clojush.pushgp.selection.epsilon-lexicase/calculate-epsilons-for-epsilon-lexicase pop-agents argmap)))


(defnk population-raw [[:config pop-agents argmap]]
  (doall (map deref pop-agents)))

(defnk next-novelty-archive [novelty-archive population-raw [:config argmap]]
  (concat novelty-archive
          (clojush.pushgp.selection.novelty/select-individuals-for-novelty-archive
            population-raw
            argmap)))

(defnk produce-new-offspring!
  [[:config
    pop-agents
    child-agents
    rand-gens
    [:argmap
     decimation-ratio
     population-size
     decimation-tournament-size
     use-single-thread
     :as argmap]]]
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

(defnk install-next-generation!
  [[:config pop-agents child-agents [:argmap population-size use-single-thread]]]
  (dotimes [i population-size]
    ((if use-single-thread swap! send)
     (nth pop-agents i)
     (fn [av] (deref (nth child-agents i)))))
  (when-not use-single-thread (apply await pop-agents))) ;; SYNCHRONIZE

(def graph
  (plumbing.graph/graph
    plush->push!
    errors!
    hah-solution-rates!
    elitegroups!
    implicit-fitness-sharing!
    novelty!
    epsilons-for-epsilon-lexicase!
    population-raw
    next-novelty-archive
    produce-new-offspring!
    install-next-generation!))
