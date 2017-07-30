(ns clojush.graphs.generation.log.csv
  (:require [plumbing.core :refer [defnk]]
            [clojure.java.io :as io]
            [clojure.data.csv :as csv]))


(defnk csv
  [[:config [:argmap csv-log-filename csv-columns print-csv-logs]]
   index population errors-n]
  (when print-csv-logs
    (let [columns (concat [:uuid]
                          (filter #(some #{%} csv-columns)
                                  [:generation :location :parent-uuids :genetic-operators
                                   :push-program-size :plush-genome-size :push-program
                                   :plush-genome :total-error :is-random-replacement]))]
      (when (zero? index)
        (with-open [csv-file (io/writer csv-log-filename :append false)]
          (csv/write-csv csv-file
                         (vector (concat (map name columns)
                                         (when (some #{:test-case-errors} csv-columns)
                                           (map #(str "TC" %)
                                                (range errors-n))))))))
      (with-open [csv-file (io/writer csv-log-filename :append true)]
        (csv/write-csv
          csv-file
          (map-indexed
            (fn [location individual]
              (concat (map (assoc individual
                             :generation index
                             :location location
                             :parent-uuids (:parent-uuids-str individual)
                             :genetic-operators (if (nil? (:genetic-operators individual))
                                                  []
                                                  (:genetic-operators individual))
                             :push-program-size (:program-n-points individual)
                             :push-program (:program-str individual)
                             :plush-genome-size (:genome-size individual)
                             :plush-genome (:genome-str individual))
                           ; This is a map of an individual
                           columns)
                      (when (some #{:test-case-errors} csv-columns)
                        (:errors individual))))
            population))))))
