(ns clojush.individual)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Individuals are records.
;; Populations are vectors of agents with individuals as their states (along with error and
;; history information).

(defrecord individual [program errors total-error hah-error rms-error history ancestors parent])

(defn make-individual [& {:keys [program errors total-error hah-error rms-error history ancestors parent]
                          :or {program nil
                               errors nil
                               total-error nil ;; a non-number is used to indicate no value
                               hah-error nil
                               rms-error nil
                               history nil
                               ancestors nil
                               parent nil}}]
  (individual. program errors total-error hah-error rms-error history ancestors parent))
