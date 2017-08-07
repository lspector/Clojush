(ns clojush.pushgp.pushgp
  (:require [clojure.java.io :as io]
            [clj-random.core :as random]
            [clojure.repl :as repl]
            [clojush.graphs.config :refer [->config]]
            [clojush.graphs.generation :refer [->generation]])

  ; to initialize registered instructions globals and to preserve random calls
  ; none of these variables are used
  (:use [clojush args globals util pushstate random individual evaluate simplification translate]
        [clojush.instructions boolean code common numbers random-instructions string char vectors
         tag zip return input-output genome]
        [clojush.pushgp breed]
        [clojush.pushgp.selection
         selection epsilon-lexicase elitegroup-lexicase implicit-fitness-sharing novelty]
        [clojush.experimental.decimation]))

(defn pushgp
  "The top-level routine of pushgp."
  ([] (pushgp '()))
  ( [args]
    (let [config (->config {:args args})
          record-time! (:record-time! config)]
      (random/with-rng (:rng config)
        (:reset-globals! config)
        (:genetic-operator-probabilities-add-to-one! config)
        (record-time! :initialization)
        (-> config :log :all!)
        (println "\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
        (println "\nGenerating initial population...") (flush)
        (loop [index 0
               novelty-archive '()]
          (let [generation (->generation {:index index
                                          :config config
                                          :novelty-archive novelty-archive})]
            (println "Processing generation:" index) (flush)
            (:plush->push! generation)
            (record-time! :reproduction)
            (print "Computing errors... ") (flush)
            (:errors! generation)
            (println "Done computing errors.") (flush)
            (record-time! :fitness)
            (:hah-solution-rates! generation)
            (:elitegroups! generation)
            (:implicit-fitness-sharing! generation)
            (:novelty! generation)
            (:epsilons-for-epsilon-lexicase! generation)
            (record-time! :other)
            (get-in generation [:log :all!])
            (case (get-in generation [:report :outcome])
              :failure
                (if (get-in config [:argmap :return-simplified-on-failure])
                   (get-in generation [:report :best :final-simplification])
                   (flush))
              :success
                (get-in generation [:report :problem-specific-report-final-simplified-best])
              :continue
                (let [next-novelty-archive (:next-novelty-archive generation)]
                  (record-time! :report)
                  (println "\nProducing offspring...") (flush)
                  (:produce-new-offspring! generation)
                  (println "Installing next generation...") (flush)
                  (:install-next-generation! generation)
                  (recur (inc index) next-novelty-archive)))))))))
