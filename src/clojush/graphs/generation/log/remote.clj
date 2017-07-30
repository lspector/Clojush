(ns clojush.graphs.generation.log.remote
  (:require [plumbing.core :refer [defnk]]
            [clojure.java.io]
            [cheshire.core]
            [cheshire.generate]
            [clojure.string])
  (:import java.net.Socket))

;; write functions as strings
(cheshire.generate/add-encoder
  clojure.lang.AFunction
  cheshire.generate/encode-str)

(def hostname-and-port (atom nil))
(def writer (atom nil))

(defn ->writer
  ; https://github.com/clojure-cookbook/clojure-cookbook/blob/master/05_network-io/5-09_tcp-client.asciidoc
  []
  (let [[hostname port] @hostname-and-port]
    (-> (java.net.Socket. hostname port)
      clojure.java.io/writer)))

(defn set-writer!
  ; Tries to get a writer to send data on, and if it fails, retries every
  ; 5 seconds
  []
  (println "Trying to connect to external server for recording at " @hostname-and-port "...")
  (try
    (reset! writer (->writer))
    (catch java.net.ConnectException _
      (Thread/sleep 5000)
      (set-writer!))))

(defn host! [host-str]
  (let [[hostname port-str] (clojure.string/split host-str #":")]
    (reset! hostname-and-port [hostname (int (bigint port-str))])
    (set-writer!)))

(defn write-data! [data]
  (when (some? @hostname-and-port)
    (println "Trying to record data to external server...")
    (try
      (do
        (cheshire.core/generate-stream data @writer)
        (.newLine @writer)
        (.flush @writer))
      (catch java.net.SocketException _
        (set-writer!)
        (write-data! data)))))

(defnk remote [[:config [:argmap record-host] :as config] population]
  (when (and record-host (not (some? @hostname-and-port)))
    (host! record-host))
  (write-data!
    {:config (dissoc config :params :args :run-uuid)
     :config-uuid (:run-uuid config)
     :individuals (map #(dissoc % :program) population)}))
