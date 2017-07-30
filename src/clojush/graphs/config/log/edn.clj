(ns clojush.graphs.config.log.edn
  (:require [plumbing.core :refer [defnk]]
            [clojure.java.io :as io]))

(defnk edn [[:argmap print-edn-logs :as argmap]]
  (when print-edn-logs
    (with-open [w (io/writer (:edn-log-filename argmap) :append false)]
      (.write w "#clojush/run")
      (.write w (prn-str (dissoc argmap
                                 ;; These keys have functions
                                 :atom-generators
                                 :error-function
                                 :problem-specific-report
                                 :random-seed))))))
