(ns clojush.individual)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Individuals are records.
;; Populations are vectors of agents with individuals as their states (along with error and
;; history information).

(defrecord individual [program errors total-error hah-error history ancestors])

(defn make-individual [& {:keys [program errors total-error hah-error history ancestors]
                          :or {program nil
                               errors nil
                               total-error nil ;; a non-number is used to indicate no value
                               hah-error nil
                               history nil
                               ancestors nil}}]
  (individual. program errors total-error hah-error history ancestors))
