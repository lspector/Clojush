(ns clojush.pushgp.pushgp
  (:require [clojure.java.io :as io]
            [clj-random.core :as random]
            [clojure.repl :as repl]
            [clojush.pushgp.record :as r])
  (:use [clojush args globals util pushstate random individual evaluate meta-errors
         simplification translate]
        [clojush.instructions boolean code common numbers random-instructions string char vectors
         tag zip environment input-output genome gtm]
        [clojush.pushgp breed report genetic-operators]
        [clojush.pushgp.selection
         selection epsilon-lexicase elitegroup-lexicase implicit-fitness-sharing novelty eliteness
         fitness-proportionate downsampled-lexicase]
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
  (mapv #(dissoc % :random-insertion) genome))

(defn make-pop-agents
  "Makes the population of agents containing the initial random individuals in the population.
   Argument is a push argmap"
  [{:keys [use-single-thread population-size
           max-genome-size-in-initial-program atom-generators genome-representation]
    :as argmap}]
  (let [population-agents (vec (repeatedly population-size
                                           #(genesis argmap)))]
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
                           (let [new-seeds (repeatedly num-remaining
                                                       #(random/lrand-bytes
                                                         (:mersennetwister random/*seed-length*)))]
                             (recur (list-concat seeds (filter ; only add seeds that we do not already have
                                                        (fn [candidate]
                                                          (not (some #(random/=byte-array % candidate)
                                                                     seeds))) new-seeds))))
                           seeds)))]
    {:random-seeds random-seeds
     :rand-gens (vec (doall (for [k (range population-size)]
                              (random/make-mersennetwister-rng (nth random-seeds k)))))}))


(defn compute-errors
  [pop-agents rand-gens novelty-archive
   {:keys [use-single-thread error-function] :as argmap}]
  (dorun (map #((if use-single-thread swap! send)
                %1 evaluate-individual error-function %2 argmap)
              pop-agents
              rand-gens))
  (when-not use-single-thread (apply await pop-agents)) ;; SYNCHRONIZE
  ;; Compute values needed for meta-errors
  ;;
  ;; calculate novelty when novelty-search or novelty meta errors are used
  (when (or (= (:parent-selection argmap) :novelty-search)
            (some #{:novelty} (:meta-error-categories argmap)))
    (calculate-novelty pop-agents novelty-archive argmap))
  ;; calculates novelty lexicase meta errors
  (when (some #{:novelty-by-case} (:meta-error-categories argmap))
    (calculate-lex-novelty pop-agents novelty-archive argmap))
  ;; ;;Novelty Lexicase based on distance
  ;; (when (some #{:novelty-by-case} (:meta-error-categories argmap))
  ;;   (calculate-lex-dist-novelty pop-agents novelty-archive argmap))
  ;;
  ;; compute meta-errors in a second pass, passing evaluated population
  (dorun (map #((if use-single-thread swap! send)
                %1 evaluate-individual-meta-errors (mapv deref pop-agents) argmap)
              pop-agents))
  (when-not use-single-thread (apply await pop-agents))) ;; SYNCHRONIZE

(defn produce-new-offspring
  [pop-agents child-agents rand-gens
   {:keys [decimation-ratio population-size decimation-tournament-size use-single-thread]
    :as argmap}]
  (let [pop (if (>= decimation-ratio 1)
              (vec (doall (map deref pop-agents)))
              (decimate (vec (doall (map deref pop-agents)))
                        (int (* decimation-ratio population-size))
                        decimation-tournament-size))
        ages (map :age pop)]
    (reset! min-age (apply min ages))
    (reset! max-age (apply max ages))
    (dotimes [i population-size]
      ((if use-single-thread swap! send)
       (nth child-agents i)
       breed
       i (nth rand-gens i) pop argmap)))
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
      (if (sequential? gen-op) ; gen-op will be sequential if it is an operator pipeline
        (doseq [g gen-op]
          (assert (contains? genetic-operators g)
                  (str "Unrecognized genetic operator: " g " in " gen-op))
          (when (= (:genome-representation argmap) :plushy)
            (assert (:works-with-plushy (get genetic-operators g))
                    (str "You are using Plushy genomes with a genetic operator that does not support Plushy genomes: " g)))
          (when (= (:genome-representation argmap) :plush)
            (assert (:works-with-plush (get genetic-operators g))
                    (str "You are using Plush genomes with a genetic operator that does not support Plush genomes: " g))))
        (do
          (assert (contains? genetic-operators gen-op)
                  (str "Unrecognized genetic operator: " gen-op))
          (when (= (:genome-representation argmap) :plushy)
            (assert (:works-with-plushy (get genetic-operators gen-op))
                    (str "You are using Plushy genomes with a genetic operator that does not support Plushy genomes: " gen-op)))
          (when (= (:genome-representation argmap) :plush)
            (assert (:works-with-plush (get genetic-operators gen-op))
                    (str "You are using Plush genomes with a genetic operator that does not support Plush genomes: " gen-op))))))
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

(defn process-generation
  "Processes the generation, returning [new novelty archive, return val],
   where new novelty archive will be nil if we are done."
  [rand-gens pop-agents child-agents generation novelty-archive
   {:keys [population-size use-single-thread] :as argmap}]
  (r/new-generation! generation)
  (println "Processing generation:" generation)
  (case (:genome-representation @push-argmap)
    :plush (population-translate-plush-to-push pop-agents @push-argmap)
    :plushy (population-translate-plushy-to-push pop-agents @push-argmap))
  (timer @push-argmap :reproduction)
  (println "Computing errors... ")
  ; select cases if using downsampled lexicase
  (when (= (:parent-selection @push-argmap) :downsampled-lexicase)
    (swap! push-argmap assoc :sub-training-cases
           (down-sample @push-argmap))
    (println "Cases for this generation:" (pr-str (:sub-training-cases @push-argmap))))
  ; compute errors
  (compute-errors pop-agents rand-gens novelty-archive @push-argmap)
  (println "Done computing errors.")
  (when (and (:preserve-frontier argmap)
             (or (not (:autoconstructive argmap))
                 (> generation 0)))
    (println "Preserving frontier... ")
    (let [filtered-candidates
          (if (:autoconstructive argmap)
            (let [diversifying-children (filter :diversifying (map deref pop-agents))
                  diversifying-frontier (filter :diversifying @frontier)
                  verified-frontier (filter (fn [parent]
                                              (some (fn [child]
                                                      (= (:parent1-genome child)
                                                         (:genome parent)))
                                                    diversifying-children))
                                            diversifying-frontier)
                  diversifying-candidates (concat diversifying-children verified-frontier)]
              (println "Number of diversifying candidates for frontier preservation:"
                       (count diversifying-candidates))
              (if (empty? diversifying-candidates)
                (map deref pop-agents)
                (if (< (count diversifying-candidates) (count pop-agents))
                  (take (count pop-agents) (cycle diversifying-candidates))
                  diversifying-candidates)))
            (concat (map deref pop-agents)
                    @frontier))]
      (if (= (count filtered-candidates) (count pop-agents))
        (do (dotimes [i population-size]
              ((if use-single-thread swap! send) (nth pop-agents i) (fn [_] (nth filtered-candidates i))))
            (when-not use-single-thread (apply await pop-agents)) ;; SYNCHRONIZE
            (reset! frontier filtered-candidates))
        (loop [preserved 0
               new-frontier []
               candidates filtered-candidates]
          (if (= preserved population-size)
            (do (dotimes [i population-size]
                  ((if use-single-thread swap! send) (nth pop-agents i) (fn [_] (nth new-frontier i))))
                (when-not use-single-thread (apply await pop-agents)) ;; SYNCHRONIZE
                (reset! frontier new-frontier))
            (let [to-preserve (select candidates argmap)
                  without-meta-errors (update-in to-preserve
                                                 [:errors]
                                                 #(vec (drop-last (count (:meta-errors to-preserve)) %)))]
              (recur (inc preserved)
                     (conj new-frontier without-meta-errors)
                     (if (= (:preserve-frontier argmap) :with-replacement)
                       candidates
                       (remove-one to-preserve candidates)))))))))
  (timer @push-argmap :fitness)
  ;; calculate solution rates if necessary for historically-assessed hardness
  (calculate-hah-solution-rates pop-agents @push-argmap)
  ;; create global structure to support elite group lexicase selection
  (when (= (:parent-selection @push-argmap) :elitegroup-lexicase)
    (build-elitegroups pop-agents @push-argmap))
  ;; calculate implicit fitness sharing fitness for population
  (when (= (:total-error-method @push-argmap) :ifs)
    (calculate-implicit-fitness-sharing pop-agents @push-argmap))
  ;; calculate epsilons for epsilon lexicase selection
  (when (and (= (:parent-selection @push-argmap) :epsilon-lexicase)
             (= (:epsilon-lexicase-version @push-argmap) :semi-dynamic)
             (= (:case-batch-size @push-argmap) 1)) ; only do this if case-batch-size is 1, since otherwise need to recalculate for every batch.
    (let [epsilons (calculate-epsilons-for-epsilon-lexicase (map deref pop-agents) @push-argmap)]
      (println "Epsilons for epsilon lexicase:" epsilons)
      (reset! epsilons-for-epsilon-lexicase epsilons)))
  (when (and (= (:parent-selection @push-argmap) :epsilon-lexicase)
             (= (:epsilon-lexicase-version @push-argmap) :static))
    (calculate-fitness-for-static-epsilon-lexicase pop-agents @push-argmap))
  ;; calculate eliteness when total-error-method necessitates
  (when (= (:total-error-method @push-argmap) :eliteness)
    (calculate-eliteness pop-agents @push-argmap))
  (timer @push-argmap :other)
  (when (= (:parent-selection @push-argmap) :fitness-proportionate)
    (calculate-fitness-proportionate-probabilities pop-agents @push-argmap))
  ;; report and check for success
  (let [[outcome best] (report-and-check-for-success (vec (doall (map deref pop-agents)))
                                                     generation @push-argmap)]
    (r/generation-data! [:outcome] outcome)
    (r/end-generation!)
    (cond (= outcome :failure) (do (printf "\nFAILURE\n")
                                   (flush)
                                   [nil
                                    (when (:return-simplified-on-failure @push-argmap)
                                      (auto-simplify best
                                                     (:error-function @push-argmap)
                                                     (:final-report-simplifications @push-argmap)
                                                     true
                                                     500
                                                     argmap))])
          (= outcome :continue) (let [next-novelty-archive
                                      (list-concat novelty-archive
                                                   (select-individuals-for-novelty-archive
                                                    (map deref pop-agents)
                                                    @push-argmap))]
                                  (when-let [del (:selection-delay @push-argmap)]
                                    (when-not (:use-single-thread @push-argmap (apply await pop-agents))) ;; SYNCHRONIZE
                                    (swap! delay-archive concat (map deref pop-agents))
                                    (when (zero? (mod generation del))
                                      (println "Performing delayed selection from archive of size: "
                                               (count @delay-archive))
                                      (flush)
                                      (doseq [a pop-agents]
                                        ((if (:use-single-thread @push-argmap) swap! send)
                                         a
                                         (fn [_] (select @delay-archive @push-argmap))))
                                      (when-not (:use-single-thread @push-argmap (apply await pop-agents))) ;; SYNCHRONIZE
                                      (reset! delay-archive [])))
                                  (timer @push-argmap :report)
                                  (println "\nProducing offspring...") (flush)
                                  (produce-new-offspring pop-agents
                                                         child-agents
                                                         rand-gens
                                                         (if (:selection-delay @push-argmap)
                                                           (assoc @push-argmap
                                                                  :parent-selection :uniform)
                                                           @push-argmap))
                                  (println "Installing next generation...") (flush)
                                  (install-next-generation pop-agents child-agents @push-argmap)
                                  [next-novelty-archive nil])
          :else [nil (final-report generation best @push-argmap)])))

(defn pushgp
  "The top-level routine of pushgp."
  ([] (pushgp '()))
  ([args]
   (reset! timer-atom (System/currentTimeMillis))
   (load-push-argmap args)
   (when (some? (:record-host @push-argmap))
     (r/host! (str (:record-host @push-argmap))))
   (random/with-rng (random/make-mersennetwister-rng (:random-seed @push-argmap))
     ;; set globals from parameters
     (reset-globals)
     (initial-report @push-argmap) ;; Print the inital report
     (r/uuid! (:run-uuid @push-argmap))
     (print-params (r/config-data! [:argmap] @push-argmap))
     (check-genetic-operator-probabilities-add-to-one @push-argmap)
     (timer @push-argmap :initialization)
     (when (:print-timings @push-argmap)
       (r/config-data! [:initialization-ms] (:initialization @timer-atom)))
     (println "\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
     (println "\nGenerating initial population...") (flush)
     (let [pop-agents (make-pop-agents @push-argmap)
           child-agents (make-child-agents @push-argmap)
           {:keys [rand-gens]} (make-rng @push-argmap)]
       (loop [generation 0
              novelty-archive '()]
         (let [[next-novelty-archive return-val]
               (process-generation rand-gens pop-agents child-agents
                                   generation novelty-archive @push-argmap)]
           (if (nil? next-novelty-archive)
             return-val
             (recur (inc generation) next-novelty-archive))))))))



