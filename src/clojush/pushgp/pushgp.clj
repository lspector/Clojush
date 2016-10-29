(ns clojush.pushgp.pushgp
  (:require [clojure.java.io :as io]
            [clj-random.core :as random]
            [clojure.repl :as repl])
  (:use [clojush args globals util pushstate random individual evaluate simplification translate]
        [clojush.instructions boolean code common numbers random-instructions string char vectors
         tag zip return input-output genome]
        [clojush.pushgp breed parent-selection report]
        [clojush.experimental.decimation]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pushgp

(defn agent-error-handler
  "Given to individual agents for handling errors."
  [agnt except]
  ;(.printStackTrace except System/out)
  ;(.printStackTrace except)
  (repl/pst except 10000)
  (System/exit 0))

(defn strip-random-insertion-flags
  "The :random-insertion flag is added to all elements of the
   genome when generated. It is used to signal that an
   instruction-map was generated randomly in the run (as opposed
   to being mutated from a parent). The individuals in generation
   0 are a special case and should not have this flag present."
  [genome]
  (map #(dissoc % :random-insertion) genome))

(defn make-pop-agents
  "Makes the population of agents containing the initial random individuals in the population.
   Argument is a push argmap"
  [{:keys [use-single-thread population-size
           max-genome-size-in-initial-program atom-generators]
    :as argmap}]
  (let [population-agents (repeatedly population-size
                                      #(make-individual
                                        :genome (strip-random-insertion-flags
                                                 (random-plush-genome max-genome-size-in-initial-program
                                                                      atom-generators
                                                                      argmap))
                                         :genetic-operators :random))]
    (mapv #(if use-single-thread
             (atom %)
             (agent % :error-handler agent-error-handler))
          population-agents)))

(defn make-child-agents
  "Makes the population of agents containing the initial random individuals in the population.
   Argument is a push argmap."
  [{:keys [use-single-thread population-size]}]
  (vec (repeatedly population-size
                   #((if use-single-thread atom agent)
                      (make-individual)
                      :error-handler agent-error-handler))))

(defn make-rng
  "Creates the random number generators used by the agents in the population.
   Argument is a push argmap"
  [{:keys [population-size]}]
  (let [random-seeds (loop [seeds '()]
                       (let [num-remaining (- population-size (count seeds))]
                         (if (pos? num-remaining)
                           (let [new-seeds (repeatedly num-remaining #(random/lrand-bytes (:mersennetwister random/*seed-length*)))]
                             (recur (concat seeds (filter (fn [candidate]
                                                            (not (some #(random/=byte-array % candidate)
                                                                       seeds))) new-seeds)))); only add seeds that we do not already have
                           seeds)))]
    {:random-seeds random-seeds
     :rand-gens (vec (doall (for [k (range population-size)]
                              (random/make-mersennetwister-rng (nth random-seeds k)))))
     }))

(defn compute-errors
  [pop-agents rand-gens {:keys [use-single-thread error-function] :as argmap}]
  (dorun (map #((if use-single-thread swap! send)
                    % evaluate-individual error-function %2 argmap)
              pop-agents
              rand-gens))
  (when-not use-single-thread (apply await pop-agents))) ;; SYNCHRONIZE

(defn produce-new-offspring
  [pop-agents child-agents rand-gens
   {:keys [decimation-ratio population-size decimation-tournament-size
           trivial-geography-radius use-single-thread ]}]
  (let [pop (if (>= decimation-ratio 1)
              (vec (doall (map deref pop-agents)))
              (decimate (vec (doall (map deref pop-agents)))
                        (int (* decimation-ratio population-size))
                        decimation-tournament-size
                        trivial-geography-radius))]
    (dotimes [i population-size]
      ((if use-single-thread swap! send)
           (nth child-agents i)
           breed
           i (nth rand-gens i) pop @push-argmap)))
  (when-not use-single-thread (apply await child-agents))) ;; SYNCHRONIZE

(defn install-next-generation
  [pop-agents child-agents {:keys [population-size use-single-thread]}]
  (dotimes [i population-size]
    ((if use-single-thread swap! send)
         (nth pop-agents i) (fn [av] (deref (nth child-agents i)))))
  (when-not use-single-thread (apply await pop-agents))) ;; SYNCHRONIZE

(defn check-genetic-operator-probabilities-add-to-one
  [argmap]
  (let [prob-map (:genetic-operator-probabilities argmap)]
    (doseq [gen-op (keys prob-map)]
      (if (sequential? gen-op)
        (doseq [g gen-op]
          (assert (contains? genetic-operators g)
                  (str "Unrecognized genetic operator: " g " in " gen-op)))
        (assert (contains? genetic-operators gen-op)
                (str "Unrecognized genetic operator: " gen-op))))
    (assert (< 0.99999
               (apply + (vals prob-map))
               1.00001)
            (str "Genetic operator probabilities do not sum to 1.0:\n"
                 (clojure.string/replace (str prob-map \newline)
                                         \,
                                         \newline)))))

(defn timer
  "Used to track the time used by different parts of evolution."
  [{:keys [print-timings]} step]
  (when print-timings
    (let [start-time @timer-atom
          current-time-for-step (get @timing-map step)]
      (reset! timer-atom (System/currentTimeMillis))
      (swap! timing-map assoc step (+ current-time-for-step (- @timer-atom start-time))))))

(defn pushgp
  "The top-level routine of pushgp."
  ([] (pushgp '()))
  ([args]
    (reset! timer-atom (System/currentTimeMillis))
    (load-push-argmap args)
    (random/with-rng (random/make-mersennetwister-rng (:random-seed @push-argmap))
      ;; set globals from parameters
      (reset-globals)
      (initial-report @push-argmap) ;; Print the inital report
      (print-params @push-argmap)
      (check-genetic-operator-probabilities-add-to-one @push-argmap)
      (timer @push-argmap :initialization)
      (println "\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
      (println "\nGenerating initial population...")
      (let [pop-agents (make-pop-agents @push-argmap)
            child-agents (make-child-agents @push-argmap)
            {:keys [rand-gens random-seeds]} (make-rng @push-argmap)]
        ;(print "Random seeds: ")
        ;(doseq [seed random-seeds] (print " " seed))
        ;(println)
        ;; Main loop
        (loop [generation 0]
          (println "Processing generation:" generation)
          (population-translate-plush-to-push pop-agents @push-argmap)
          (timer @push-argmap :reproduction)
          (print "Computing errors... ")
          (compute-errors pop-agents rand-gens @push-argmap)
          (println "Done computing errors.")
          (timer @push-argmap :fitness)
          ;; calculate solution rates if necessary for historically-assessed hardness
          (calculate-hah-solution-rates pop-agents @push-argmap)
          ;; create global structure to support elite group lexicase selection
          (when (= (:parent-selection @push-argmap) :elitegroup-lexicase)
            (build-elitegroups pop-agents))
          ;; calculate implicit fitness sharing fitness for population
          (when (= (:total-error-method @push-argmap) :ifs)
            (calculate-implicit-fitness-sharing pop-agents @push-argmap))
          (timer @push-argmap :other)
          ;; report and check for success
          (let [[outcome best] (report-and-check-for-success (vec (doall (map deref pop-agents)))
                                                             generation @push-argmap)]
            (cond (= outcome :failure) (do (printf "\nFAILURE\n")
                                         (if (:return-simplified-on-failure @push-argmap)
                                           (auto-simplify best (:error-function @push-argmap) (:final-report-simplifications @push-argmap) true 500)
                                           (flush)))
                  (= outcome :continue) (do (timer @push-argmap :report)
                                          (println "\nProducing offspring...")
                                          (produce-new-offspring pop-agents child-agents rand-gens @push-argmap)
                                          (println "Installing next generation...")
                                          (install-next-generation pop-agents child-agents @push-argmap)
                                          (recur (inc generation)))
                  :else  (final-report generation best @push-argmap))))))))
