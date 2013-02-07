(ns analysis.core
  (:import java.lang.Math)
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [cosmos.core :as cosmos]))

(defrecord
  ;;"Data structure for defining a single log."
  Log [insts params reports summary])

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
    (let [reports (for [chunk (remove pred (partition-by pred reports-chunk))]
                    (->> chunk
                         (map #(s/split % #":"))
                         (remove #(not= 2 (count %)))
                         (flatten)
                         (map s/trim)
                         (apply sorted-map)))
          max-attributes (apply max (map count reports))]
      (filter #(= max-attributes (count %)) reports))))

(defn- make-summary [summary-chunk]
  (let [outcome (re-find #"SUCCESS|FAILURE" (first summary-chunk))
        final-gen (re-find #"[0-9]+" (first summary-chunk))
        smap (->> summary-chunk
                  (map #(s/split % #":"))
                  (filter #(= 2 (count %)))
                  (flatten)
                  (map s/trim)
                  (apply sorted-map))]
    (assoc smap "outcome" outcome "final-gen" final-gen)))

(defn parse-Log
  "Parses a single log file (i.e. output of a Clojush run) into a Log data structure"
  [log-file]
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
  
(defn parse-Logs
  "parses the contents of a log folder into a Log data structure, assuming that all files in the folder are logs"
  [log-folder]
  (for [log (.listFiles (io/file log-folder))]
    (parse-Log log)))

(defn Y
  "Koza's Y - `point probability of success'"
  [logs]
  (assert (reduce #(and (= %1 %2) %1) (map #(get (:params %) "population-size") logs)))
  (let [success-array (int-array (read-string (get (:params (first logs)) "population-size")) 0)]
    (->> (reduce #(let [summary (:summary %2)]
                    (when (= "SUCCESS" (get summary "outcome"))
                      (let [index (read-string (get summary "final-gen"))]
                        (aset %1 index (inc (aget %1 index)))))                   
                    %1)
                 success-array logs)
         (map #(/ % (count logs))))))


(defn P
  "Koza's P - `cumulative probability of success'"
  [logs]
  (let [point-probs (to-array (Y logs))]
    (doseq [i (rest (range (alength point-probs)))]
      (aset point-probs i (+ (aget point-probs i)
                             (aget point-probs (dec i)))))
    (seq point-probs)))


(defn R
  "Koza's R - `number of independent runs needed get a success with probability z"
  [logs z]  
  (let [enumerate (fn [lst] (map #(vector %1 %2) (range (count lst)) lst))
        cumulative-probs (P logs)
        calcs (map #(Math/ceil
                     (/ (Math/log (- 1 z))
                        (Math/log (- 1 %))))
                   cumulative-probs)]
    (ffirst (sort #(> (%1 1) (%2 1)) (enumerate calcs)))))

  
(defn CE
  "Koza's Computational Effort"
  [logs z]
  (* (R logs z)
     (count logs)
     (read-string (get (:params (first logs)) "population-size"))))


(defn MBF
  "Mean Best Fitness"
  [logs error-key]
  (let [best-fitnesses (->> (for [log logs]
                              (for [report (:reports log)]
                                (get report error-key)))
                            flatten
                            (remove nil?)
                            (map read-string)
                            flatten)]
    (/ (apply + best-fitnesses) (count best-fitnesses))))

(defn cosmos
  "Returns a summary of the recommended runs for this data set"
  [logs]
  (cosmos/recommended-runs (-> (for [log logs]
                                 (for [[report gen] (map #(vector %1 %2) (:reports log) (iterate inc 0))]
                                   (for [[ord val] (seq (get report "Cosmos Data:"))]
                                     (struct-map cosmos/cosmos-data
                                       :ord ord
                                       :gen gen
                                       :val val))))
                               flatten)))
                               