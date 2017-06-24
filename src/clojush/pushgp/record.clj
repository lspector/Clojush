;;; Records the results of runs to an external server

;; Use documented in https://push-language.hampshire.edu/t/recording-and-analyzing-experimental-results/830

;; If `record-host` is set in the arguments, then we should send
;; send data about each run, as it progresses, to that host for archival
;; and monitoring purposes.

;; The functions in this file are stateful and should be called in this order:
;;
;; (new-run! uuid! config-data!* (new-generation! generation-data!* end-generation!)*)*
;;
;; Currently it doesn't enforce this and if you call a method when you shouldn't
;; the results are unkown.
;; Also it will not send anything over the network until `host!` is called,
;; before that, `end-generation!` will be a no-op.

(ns clojush.pushgp.record
  (:require [clojure.java.io]
            [cheshire.core]
            [cheshire.generate]
            [clojure.string]))

;; write functions as strings
(cheshire.generate/add-encoder
  clojure.lang.AFunction
  cheshire.generate/encode-str)

(def hostname-and-port (atom nil))
(def writer (atom nil))

(defn- ->writer
  ; https://github.com/clojure-cookbook/clojure-cookbook/blob/master/05_network-io/5-09_tcp-client.asciidoc
  []
  (let [[hostname port] @hostname-and-port]
    (-> (java.net.Socket. hostname port)
      clojure.java.io/writer)))

(defn- set-writer!
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

(defn- write-data! [data]
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

(def data (atom {}))


;; Stores a configuration option for the run, for the sequence of `ks` and value `v`
;; i.e. (config-data! [:git-uuid] "abc-def")
(defn config-data! [ks v]
  (swap! data assoc-in (cons :config ks) v)
  v)

(defn seconds-since-epoch
  ;; http://stackoverflow.com/a/17432411
  ;; because Spark interprets numbers as dates in this format when in JSON
  []
  (quot (System/currentTimeMillis) 1000))

;; Resets the run data and saves the start time. Should be called at the
;; begining of a run
(defn new-run! []
  (reset! data {:config {:start-time (seconds-since-epoch)}}))

(defn uuid! [uuid]
  (swap! data assoc :uuid uuid))

;; Resets the generation data and should be called at the begining of 
;; each generation
(defn new-generation! [index]
  (swap!
    data
    assoc
    :index index
    :generation {:start-time (seconds-since-epoch)}))
      

;; Stores data about the generation, i.e.
;; (generation-data! [:best :error] [1 2 3 10])
(defn generation-data! [ks v]
  (swap! data assoc-in (cons :generation ks) v)
  v)

;; Sends the data for the current generation over the network to be recorded
;; Also sends the configuration with each generation
(defn end-generation! []
  (let [{:keys [generation uuid index config]} @data]
    (write-data!
      (assoc generation
        :config-uuid uuid
        :index index
        :config config))))
