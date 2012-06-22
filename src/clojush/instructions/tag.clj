(ns clojush.instructions.tag
  (:use [clojush.pushstate]
        [clojush.globals]
        [clojush.random])
  (:require [clojure.string :as string]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag pseudo-instructions

(defn tag-instruction? 
  [i]
  (and (symbol? i) 
       (or
         (.startsWith (name i) "tag")
         (.startsWith (name i) "untag"))))

(defn closest-association
  "Returns the key-val pair for the closest match to the given tag
   in the given state."
  [tag state]
  (loop [associations (conj (vec (:tag state)) (first (:tag state)))] ;; conj does wrap
    (if (or (empty? (rest associations))
            (<= tag (ffirst associations)))
      (first associations)
      (recur (rest associations)))))

(defn handle-tag-instruction
  "Executes the tag instruction i in the state. Tag instructions take one of
   the following forms:
   tag_<type>_<number> 
   create tage/value association, with the value taken from the stack
   of the given type and the number serving as the tag
   untag_<number>
   remove the association for the closest-matching tag
   tagged_<number> 
   push the value associated with the closest-matching tag onto the
   exec stack (or no-op if no associations).
   tagged_code_<number> 
   push the value associated with the closest-matching tag onto the
   code stack (or no-op if no associations).
   tagged_when_<number>
   requires a boolean; if true pushes the value associated with the
   closest-matching tag onto the exec stack (or no-op if no boolean
   or no associations).
   "
  [i state]
  (let [iparts (string/split (name i) #"_")]
    (cond
      ;; if it's of the form tag_<type>_<number>: CREATE TAG/VALUE ASSOCIATION
      (= (first iparts) "tag") 
      (let [source-type (read-string (str ":" (nth iparts 1)))
            the-tag (read-string (nth iparts 2))]
        (if (empty? (source-type state))
          state
          ((if @global-pop-when-tagging pop-item (fn [type state] state))
               source-type
               (assoc state :tag (assoc (or (:tag state) (sorted-map))
                                        the-tag 
                                        (first (source-type state)))))))
      ;; if it's of the form untag_<number>: REMOVE TAG ASSOCIATION
      (= (first iparts) "untag")
      (if (empty? (:tag state))
        state
        (let [the-tag (read-string (nth iparts 1))]
          (assoc state :tag (dissoc (:tag state) (first (closest-association the-tag state))))))
      ;; if we get here it must be one of the retrieval forms starting with "tagged_", so 
      ;; we check to see if there are assocations and consider the cases if so
      :else
      (if (empty? (:tag state))
        state ;; no-op if no associations
        (cond ;; it's tagged_code_<number>
              (= (nth iparts 1) "code") 
              (let [the-tag (read-string (nth iparts 2))]
                (push-item (second (closest-association the-tag state)) :code state))
              ;; it's tagged_when_<number>
              (= (nth iparts 1) "when") 
              (if (empty? (:boolean state))
                state
                (if (= true (first (:boolean state)))
                  (let [the-tag (read-string (nth iparts 2))]
                    (push-item (second (closest-association the-tag state))
                               :exec (pop-item :boolean state)))
                  (pop-item :boolean state)))
              ;; else it's just tagged_<number>, result->exec
              :else
              (let [the-tag (read-string (nth iparts 1))]
                (push-item (second (closest-association the-tag state)) :exec state)))))))

(defn tag-instruction-erc
  "Returns a function which, when called on no arguments, returns a symbol of the form
   tag_<type>_<number> where type is one of the specified types and number is in the range 
   from 0 to the specified limit (exclusive)."
  [types limit]
  (fn [] (symbol (str "tag_"
                      (name (lrand-nth types))
                      "_"
                      (str (lrand-int limit))))))

(defn untag-instruction-erc
  "Returns a function which, when called on no arguments, returns a symbol of the form
   untag_<number> where number is in the range from 0 to the specified limit (exclusive)."
  [limit]
  (fn [] (symbol (str "untag_"
                      (str (lrand-int limit))))))

(defn tagged-instruction-erc
  "Returns a function which, when called on no arguments, returns a symbol of the form
   tagged_<number> where number is in the range from 0 to the specified limit (exclusive)."
  [limit]
  (fn [] (symbol (str "tagged_"
                      (str (lrand-int limit))))))

(defn tagged-code-instruction-erc
  "Returns a function which, when called on no arguments, returns a symbol of the form
   tagged_code_<number> where number is in the range from 0 to the specified limit (exclusive)."
  [limit]
  (fn [] (symbol (str "tagged_code_"
                      (str (lrand-int limit))))))

(defn tagged-when-instruction-erc
  "Returns a function which, when called on no arguments, returns a symbol of the form
   tagged_when_<number> where number is in the range from 0 to the specified limit (exclusive)."
  [limit]
  (fn [] (symbol (str "tagged_when_"
                      (str (lrand-int limit))))))
