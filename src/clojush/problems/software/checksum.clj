

(defn checksum [st]
  (char (+ (mod (apply + (map int st)) 64)
           (int \space))))

; Answer should be \V
(checksum "?? water the plants !!")
