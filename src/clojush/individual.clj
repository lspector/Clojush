(ns clojush.individual)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Individuals are records.
;; Populations are vectors of agents with individuals as their states (along with error and
;; history information).

(defrecord individual [genome program errors behaviors total-error normalized-error weighted-error
                       novelty meta-errors history ancestors uuid parent-uuids genetic-operators
                       age grain-size is-random-replacement])

(defn make-individual [& {:keys [genome program errors behaviors total-error normalized-error weighted-error
                                 novelty meta-errors history ancestors uuid parent-uuids
                                 genetic-operators age grain-size is-random-replacement]
                          :or {genome nil
                               program nil
                               errors nil
                               behaviors nil
                               total-error nil ;; a non-number is used to indicate no value
                               normalized-error nil
                               weighted-error nil
                               novelty nil
                               meta-errors nil
                               history nil
                               ancestors nil
                               uuid (java.util.UUID/randomUUID)
                               parent-uuids nil
                               genetic-operators nil
                               age 0
                               grain-size 1 ; used for random-screen
                               is-random-replacement false
                               }}]
  (individual. genome program errors behaviors total-error normalized-error weighted-error novelty
               meta-errors history ancestors uuid parent-uuids genetic-operators age grain-size
               is-random-replacement))

(defn printable [thing]
  (letfn [(unlazy [[head & tail]]
                  (cons (if (seq? head) (unlazy head) head)
                        (if (nil? tail) tail (unlazy tail))))]
    (cond (seq? thing) (unlazy thing)
          (nil? thing) 'nil
          :else thing)))

(defn individual-string [i]
  (cons 'individual.
        (let [k '(:genome :program :errors :behaviors :total-error :normalized-error 
                          :weighted-error :novelty :meta-errors :history :ancestors :uuid 
                          :parent-uuids :genetic-operators :age :grain-size 
                          :is-random-replacement)]
          (interleave k  (map #(printable (get i %)) k)))))

