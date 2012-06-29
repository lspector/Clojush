(ns clojush.experimental.tagged-code-macros
  (:use [clojush.util]
        [clojush.random]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tagged-code macros

(defn tagged-code-macro?
  "Retruns true if i is a tagged-code macro call."
  [i]
  (and (map? i)
       (:tagged_code_macro i)))

(defn handle-tag-code-macro
  "Given a tagged-code macro call and a push state, this returns the push state with the
   call expanded on the exec stack."
  [i state]
  (if (and (not (empty? (:argument_tags i))) (empty? (:tag state)))
    state
    (assoc state :exec
           (concat (concat
                     ;; possibly grab arguments from tag space and push them on the code stack
                     (map #(symbol (str "tagged_code_" (str %))) (:argument_tags i))
                     ;; push additional args, if any
                     (:additional_args i)
                     ;; execute the code instruction
                     (list (:instruction i))
                     ;; possibly tag results
                     (map #(symbol (str "tag_code_" (str %))) (:result_tags i))
                     )
                   (:exec state)))))

(defn tagged-code-macro-erc
  "Returns a function which, when called on no arguments, returns a tagged-code macro,
   which is a map."
  ([instruction tag-limit num-argument-tags num-result-tags additional-arg-generator]
    (fn [] {:tagged_code_macro true :instruction instruction
            :argument_tags (repeatedly num-argument-tags #(lrand-int tag-limit))
            :additional_args (additional-arg-generator)
            :result_tags (repeatedly num-result-tags #(lrand-int tag-limit))}))
  ([instruction tag-limit num-argument-tags num-result-tags]
    (tagged-code-macro-erc instruction tag-limit num-argument-tags num-result-tags (fn [] ()))))

(defn abbreviate-tagged-code-macros
  "Returns a copy of program with macros abbreviated as symbols. The returned program will
   not run as-is."
  [program]
  (postwalklist (fn [item]
                  (if (tagged-code-macro? item)
                    (symbol (str "TC_"
                                 (:instruction item)
                                 (print-str (:argument_tags item))
                                 (print-str (:result_tags item))
                                 (if (empty? (:additional_args item)) 
                                   "" 
                                   (print-str (:additional_args item)))))
                    item))
                program))
