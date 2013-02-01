(ns analysis.core
  (:require [clojure.java.io :as io]
            [cosmos.core :as cosmos]))

(defrecord Log [insts params reports summary])

(defn- make-inst [inst-chunk]
  (-> inst-chunk
      (s/split #":")
      (second)
      (read-string)))

(defn- make-params [param-chunk]
  (->> param-chunk
       (map #(s/split % #"="))
       (map #(vector (s/trim (% 0)) (s/trim (% 1))))
       (reduce #(assoc %1 (%2 0) (%2 1)) (sorted-map))))

(defn- make-reports [reports-chunk]
  (let [pred #(.contains % ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")]   
    (for [chunk (remove pred (partition-by pred reports-chunk))]
      (->> chunk
           (map #(s/split % #":"))
           (remove #(not= 2 (count %)))
           (flatten)
           (map s/trim)
           (apply sorted-map)))))

(defn- make-summary [summary-chunk]
  (->> summary-chunk
       (map #(s/split % #":"))
       (filter #(= 2 (count %)))
       (flatten)
       (map s/trim)
       (apply sorted-map)))
      

(defn parse-Log [log-file]
  (let [lines (s/split-lines (slurp log-file))
        inst-chunk (first (filter #(.contains % "Registered instructions:") lines))
        param-chunk (filter #(.contains % "=") lines)
        reports-chunk (->> lines
                           (drop-while #(not (.contains % ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")))
                           (take-while #(and (not (.contains % "SUCCESS")) (not (.contains % "FAILURE")))))
        summary-chunk (drop-while #(and (not (.contains % "SUCCESS")) (not (.contains % "FAILURE"))) lines)]
    (Log. (make-inst inst-chunk)
          (make-params param-chunk)
          (make-reports reports-chunk)
          (make-summary summary-chunk))))

  
(defn aggregate-logs [log-folder]
  (for [log (.listFiles (io/file log-folder))]
    (parse-Log log)))