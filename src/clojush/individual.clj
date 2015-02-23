(ns clojush.individual
  (:require [clj-uuid :as uuid]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Individuals are records.
;; Populations are vectors of agents with individuals as their states (along with error and
;; history information).

(defrecord individual [genome program errors total-error normalized-error weighted-error meta-errors history ancestors parent uuid parent-uuids genetic-operators])

(defn make-individual [& {:keys [genome program errors total-error normalized-error weighted-error meta-errors history ancestors parent uuid parent-uuids genetic-operators]
                          :or {genome nil
                               program nil
                               errors nil
                               total-error nil ;; a non-number is used to indicate no value
                               normalized-error nil
                               weighted-error nil
                               meta-errors nil
                               history nil
                               ancestors nil
                               parent nil
                               uuid (uuid/v4)
                               parent-uuids nil
                               genetic-operators nil}}]
  (individual. genome program errors total-error normalized-error weighted-error meta-errors history ancestors parent uuid parent-uuids genetic-operators))

(defn printable [thing]
  (letfn [(unlazy [[head & tail]]
                  (cons (if (seq? head) (unlazy head) head)
                        (if (nil? tail) tail (unlazy tail))))]
    (cond (seq? thing) (unlazy thing)
          (nil? thing) 'nil
          :else thing)))

(defn individual-string [i]
  (cons 'individual.
        (let [k '(:genome :program :errors :total-error :normalized-error :weighted-error :meta-errors :history :ancestors :parent :uuid :parent-uuids :genetic-operators)]
          (interleave k  (map #(printable (get i %)) k)))))
