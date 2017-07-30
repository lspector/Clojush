(ns clojush.graphs.generation.log.edn
  (:require [plumbing.core :refer [defnk]]
            [clojure.java.io :as io]))

(defnk edn
  [[:config argmap]
   index
   population]
  (when (:print-edn-logs argmap)
    (with-open [w (io/writer (:edn-log-filename argmap) :append true)] ;; Opens and closes the file once per call
      (doall
       (map-indexed (fn [i individual]
                     (let [additional-data {:generation index
                                            :location i
                                            :push-program-size (:program-size individual)
                                            :plush-genome-size (:genome-size individual)}]
                       (.write w "#clojush/individual")
                       (.write w (prn-str (merge
                                           (select-keys additional-data (:edn-additional-keys argmap))
                                           (select-keys individual (:edn-keys argmap)))))))
            population)))))
