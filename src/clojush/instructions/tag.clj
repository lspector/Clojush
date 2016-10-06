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
         (.startsWith (name i) "untag")
         (.startsWith (name i) "return_tag_"))))

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
   return_tag_<type>_<number>
   pushes (item_from_<type>_stack tag_<type>_<number>) onto the return stack.
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
      ;; if it's return_tag_<type>_<number>: Push
      ;; (item_from_<type>_stack tag_<type>_<number>) onto the return stack. Pop the
      ;; item if @global-pop-when-tagging
      (and (= (first iparts) "return")
           (= (second iparts) "tag"))
      (let [source-type (read-string (str ":" (nth iparts 2)))
            the-tag (read-string (nth iparts 3))
            new-tag-instr (symbol (subs (name i) (count "return_")))]
        (if (empty? (source-type state))
          state
          (let [item (list (top-item source-type state) new-tag-instr)]
            ((if @global-pop-when-tagging pop-item (fn [type state] state))
                 source-type
                 (push-item item :return state)))))
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
  ([types limit]
    (fn [] (symbol (str "tag_"
                        (name (lrand-nth types))
                        "_"
                        (str (lrand-int limit))))))
  ([types] (tag-instruction-erc types @global-tag-limit)))
  
(defn untag-instruction-erc
  "Returns a function which, when called on no arguments, returns a symbol of the form
   untag_<number> where number is in the range from 0 to the specified limit (exclusive)."
  ([limit]
    (fn [] (symbol (str "untag_"
                        (str (lrand-int limit))))))
  ([] (untag-instruction-erc @global-tag-limit)))
  
(defn tagged-instruction-erc
  "Returns a function which, when called on no arguments, returns a symbol of the form
   tagged_<number> where number is in the range from 0 to the specified limit (exclusive)."
  ([limit]
    (fn [] (symbol (str "tagged_"
                        (str (lrand-int limit))))))
  ([] (tagged-instruction-erc @global-tag-limit)))
  
(defn tagged-code-instruction-erc
  "Returns a function which, when called on no arguments, returns a symbol of the form
   tagged_code_<number> where number is in the range from 0 to the specified limit (exclusive)."
  ([limit]
    (fn [] (symbol (str "tagged_code_"
                        (str (lrand-int limit))))))
  ([] (tagged-code-instruction-erc @global-tag-limit)))

(defn tagged-when-instruction-erc
  "Returns a function which, when called on no arguments, returns a symbol of the form
   tagged_when_<number> where number is in the range from 0 to the specified limit (exclusive)."
  ([limit]
    (fn [] (symbol (str "tagged_when_"
                        (str (lrand-int limit))))))
  ([] (tagged-when-instruction-erc @global-tag-limit)))

(defn return-tag-instruction-erc
  "Returns a function which, when called on no arguments, returns a function of Push
   state that pushes a literal followed by a tagging instruction of the same type."
  ([types limit]
    (fn [] (symbol (str "return_tag_"
                        (name (lrand-nth types))
                        "_"
                        (str (lrand-int limit))))))
  ([types] (return-tag-instruction-erc types @global-tag-limit)))

(define-registered
  integer_tagged_instruction
  ^{:stack-types [:integer :tag :exec]}
  (fn [state]
    (if (not (empty? (:integer state)))
      (push-item (symbol (str "tagged_" (mod (#(if (neg? %) (= %) %) (stack-ref :integer 0 state))
                                             @global-tag-limit)))
                 :exec
                 (pop-item :integer state))
      state)))

(define-registered
  integer_untag_instruction
  ^{:stack-types [:integer :tag :exec]}
  (fn [state]
    (if (not (empty? (:integer state)))
      (push-item (symbol (str "untag_" (mod (#(if (neg? %) (= %) %) (stack-ref :integer 0 state))
                                            @global-tag-limit)))
                 :exec
                 (pop-item :integer state))
      state)))

(define-registered
  integer_tag_exec_instruction
  ^{:stack-types [:integer :tag :exec]}
  (fn [state]
    (if (not (empty? (:integer state)))
      (push-item (symbol (str "tag_exec_" (mod (#(if (neg? %) (= %) %) (stack-ref :integer 0 state))
                                               @global-tag-limit)))
                 :exec
                 (pop-item :integer state))
      state)))

(define-registered
  integer_tag_code_instruction
  ^{:stack-types [:integer :tag :exec :code]}
  (fn [state]
    (if (not (empty? (:integer state)))
      (push-item (symbol (str "tag_code_" (mod (#(if (neg? %) (= %) %) (stack-ref :integer 0 state))
                                               @global-tag-limit)))
                 :exec
                 (pop-item :integer state))
      state)))

(define-registered
  integer_tag_integer_instruction
  ^{:stack-types [:integer :tag :exec]}
  (fn [state]
    (if (not (empty? (:integer state)))
      (push-item (symbol (str "tag_integer_" (mod (#(if (neg? %) (= %) %) (stack-ref :integer 0 state))
                                                  @global-tag-limit)))
                 :exec
                 (pop-item :integer state))
      state)))

(define-registered
  integer_tag_float_instruction
  ^{:stack-types [:integer :tag :exec :float]}
  (fn [state]
    (if (not (empty? (:integer state)))
      (push-item (symbol (str "tag_float_" (mod (#(if (neg? %) (= %) %) (stack-ref :integer 0 state))
                                                @global-tag-limit)))
                 :exec
                 (pop-item :integer state))
      state)))

(define-registered
  integer_tag_boolean_instruction
  ^{:stack-types [:integer :tag :exec :boolean]}
  (fn [state]
    (if (not (empty? (:integer state)))
      (push-item (symbol (str "tag_boolean_" (mod (#(if (neg? %) (= %) %) (stack-ref :integer 0 state))
                                                  @global-tag-limit)))
                 :exec
                 (pop-item :integer state))
      state)))

(define-registered
  integer_tag_char_instruction
  ^{:stack-types [:integer :tag :exec :char]}
  (fn [state]
    (if (not (empty? (:integer state)))
      (push-item (symbol (str "tag_char_" (mod (#(if (neg? %) (= %) %) (stack-ref :integer 0 state))
                                               @global-tag-limit)))
                 :exec
                 (pop-item :integer state))
      state)))

(define-registered
  integer_tag_string_instruction
  ^{:stack-types [:integer :tag :exec :string]}
  (fn [state]
    (if (not (empty? (:integer state)))
      (push-item (symbol (str "tag_string_" (mod (#(if (neg? %) (= %) %) (stack-ref :integer 0 state))
                                                 @global-tag-limit)))
                 :exec
                 (pop-item :integer state))
      state)))

(define-registered
  integer_tag_zip_instruction
  ^{:stack-types [:integer :tag :exec :zip]}
  (fn [state]
    (if (not (empty? (:integer state)))
      (push-item (symbol (str "tag_zip_" (mod (#(if (neg? %) (= %) %) (stack-ref :integer 0 state))
                                              @global-tag-limit)))
                 :exec
                 (pop-item :integer state))
      state)))

(define-registered
  integer_tag_genome_instruction
  ^{:stack-types [:integer :tag :exec :genome]}
  (fn [state]
    (if (not (empty? (:integer state)))
      (push-item (symbol (str "tag_genome_" (mod (#(if (neg? %) (= %) %) (stack-ref :integer 0 state))
                                                 @global-tag-limit)))
                 :exec
                 (pop-item :integer state))
      state)))
