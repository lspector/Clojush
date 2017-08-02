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
    (let [config (->config {:args args})]
      (random/with-rng (:rng config)
        (:assert-genetic-operator-probabilities-add-to-one config)
        (-> config :log :all!)
        (loop [index 0
               novelty-archive '()]
          (let [generation
                (->generation
                 {:index index
                  :config config
                  :novelty-archive novelty-archive})
                report (:report generation)]
            (-> generation :log :all!)
            (case (:outcome report)
              :failure
                (if (:return-simplified-on-failure (:argmap config))
                   (get-in report [:best :final-simplification])
                   (flush))
              :success
                (:problem-specific-report-final-simplified-best report)
              :continue
                (let [next-novelty-archive (:next-novelty-archive generation)]
                  (:install-next-generation generation)
                  (recur (inc index) next-novelty-archive)))))))))
