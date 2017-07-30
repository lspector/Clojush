(ns clojush.graphs.config
  (:require [plumbing.core :refer [defnk]]
            [plumbing.graph]
            [clj-random.core :as random]
            [local-file]
            [clojure.string :as string]
            [schema.core :as s]
            [plumbing.fnk.schema :refer [guess-expr-output-schema]]
            [plumbing.fnk.pfnk :as pfnk]

            [clojush.args]
            [clojush.pushstate]
            [clojush.pushgp.breed :refer [genetic-operators]]
            [clojush.graphs.config.log]
            [clojush.graphs.config.population]
            [clojush.graphs.utils :refer [compile-graph]])
  (:import (java.io IOException)))

(defnk clojush-version []
  (try
    (let [version-str (apply str (butlast (re-find #"\".*\""
                                                   (first (string/split-lines
                                                            (local-file/slurp* "project.clj"))))))]
      (.substring version-str 1 (count version-str)))
    ;; on a windows machine, starting from the repl, local-file fails
    (catch Exception _
      nil)))


(defnk argmap-without-return-type [args timer-atom]
  (clojush.args/load-push-argmap args)
  @clojush.args/push-argmap)

; need to manually compute and annotate return type
; otherwise wont be able to destructure argmap
; https://github.com/plumatic/plumbing/issues/117
(def argmap
  (pfnk/fn->fnk
     argmap-without-return-type
     'argmap
     [(pfnk/input-schema argmap-without-return-type)
      (guess-expr-output-schema @clojush.args/push-argmap)]))

(defnk argmap-with-random-str [argmap]
  (update argmap :random-seed random/seed-to-string))

(defnk git-hash []
  (try
    (let [dir (local-file/project-dir)]
      (string/trim
        (slurp
          (str dir
               "/.git/"
               (subs
                 (string/trim
                   (slurp
                     (str dir "/.git/HEAD")))
                 5)))))
    (catch IOException _
      nil)))

(defnk timer-atom []
  (atom (System/currentTimeMillis)))

(defnk timing-map []
  (atom {:initialization 0 :reproduction 0 :report 0 :fitness 0 :other 0}))

(defnk record-time! [timer-atom timing-map argmap]
  (fn [step]
    (when (:print-timings argmap)
      (let [start-time @timer-atom
            current-time-for-step (get @timing-map step)]
        (reset! timer-atom (System/currentTimeMillis))
        (swap! timing-map assoc step (+ current-time-for-step (- @timer-atom start-time)))))))

(defnk assert-genetic-operator-probabilities-add-to-one
  [reset-globals ;; for same order
   record-time!
   argmap]
  (let [gop (:genetic-operator-probabilities argmap)]
    (doseq [gen-op (keys gop)]
      (if (sequential? gen-op)
        (doseq [g gen-op]
          (assert (contains? genetic-operators g)
                  (str "Unrecognized genetic operator: " g " in " gen-op)))
        (assert (contains? genetic-operators gen-op)
                (str "Unrecognized genetic operator: " gen-op))))
    (assert (< 0.99999
               (apply + (vals gop))
               1.00001)
            (str "Genetic operator probabilities do not sum to 1.0:\n"
                 (clojure.string/replace (str gop \newline)
                                         \,
                                         \newline))))
  (record-time! :initialization))

(defnk reset-globals []
  (clojush.args/reset-globals))

(defnk initialization-ms [timing-map]
  (:initialization @timing-map))

(defnk rng [argmap]
  (random/make-mersennetwister-rng (:random-seed argmap)))

(defnk registered-instructions []
  @clojush.pushstate/registered-instructions)


(defnk rand-gens
  "Creates the random number generators used by the agents in the population."
  [child-agents
   argmap]
  (let [pop-size (:population-size argmap)]
    (let [random-seeds (loop [seeds '()]
                         (let [num-remaining (- pop-size (count seeds))]
                           (if (pos? num-remaining)
                             (let [new-seeds (repeatedly num-remaining
                                                         #(random/lrand-bytes
                                                            (:mersennetwister random/*seed-length*)))]
                               (recur (concat seeds (filter ; only add seeds that we do not already have
                                                      (fn [candidate]
                                                        (not (some #(random/=byte-array % candidate)
                                                                   seeds))) new-seeds))))
                             seeds)))]
      (vec (doall (for [k (range pop-size)]
                    (random/make-mersennetwister-rng (nth random-seeds k))))))))

(def graph
  (plumbing.graph/graph
    clojush-version
    timer-atom
    argmap
    argmap-with-random-str
    registered-instructions
    git-hash
    timing-map
    initialization-ms
    reset-globals
    record-time!
    assert-genetic-operator-probabilities-add-to-one
    rng
    clojush.graphs.config.population/graph
    rand-gens
    :log clojush.graphs.config.log/graph))


(def ->config
  (compile-graph :config graph))
