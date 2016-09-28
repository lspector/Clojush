(ns clojush.instructions.string
  (:use [clojush pushstate globals]
        [clojure.string :only [split trim]]
        clojush.instructions.vectors)
  (:require [clojure.string :as string]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; instructions for strings

(define-registered
  string_frominteger
  ^{:stack-types [:string :integer]}
  (fn [state]
    (if (not (empty? (:integer state)))
      (let [item (stack-ref :integer 0 state)]
        (->> (pop-item :integer state)
             (push-item (str item) :string)))
      state)))

(define-registered
  string_fromfloat
  ^{:stack-types [:string :float]}
  (fn [state]
    (if (not (empty? (:float state)))
      (let [item (stack-ref :float 0 state)]
        (->> (pop-item :float state)
             (push-item (str item) :string)))
      state)))

(define-registered
  string_fromboolean
  ^{:stack-types [:string :boolean]}
  (fn [state]
    (if (not (empty? (:boolean state)))
      (let [item (stack-ref :boolean 0 state)]
        (->> (pop-item :boolean state)
             (push-item (str item) :string)))
      state)))

(define-registered
  string_fromchar
  ^{:stack-types [:string :char]}
  (fn [state]
    (if (not (empty? (:char state)))
      (let [item (stack-ref :char 0 state)]
        (->> (pop-item :char state)
             (push-item (str item) :string)))
      state)))

(define-registered
  string_concat
  ^{:stack-types [:string]}
  (fn [state]
    (if (not (empty? (rest (:string state))))
      (if (>= max-string-length (+ (count (stack-ref :string 1 state))
                                   (count (stack-ref :string 0 state))))
        (push-item (str (stack-ref :string 1 state)
                        (stack-ref :string 0 state))
                   :string
                   (pop-item :string (pop-item :string state)))
        state)
      state)))

(define-registered
  string_conjchar ; Conj char onto string
  ^{:stack-types [:string :char]}
  (fn [state]
    (if (and (not (empty? (:string state)))
             (not (empty? (:char state))))
      (let [result (str (stack-ref :string 0 state) (stack-ref :char 0 state))]
        (if (>= max-string-length (count result))
          (push-item result
                     :string
                     (pop-item :char (pop-item :string state)))
          state))
      state)))

(define-registered
  string_take
  ^{:stack-types [:string :integer]}
  (fn [state]
    (if (and (not (empty? (:string state)))
             (not (empty? (:integer state))))
      (push-item (apply str (take (stack-ref :integer 0 state)
                                  (stack-ref :string 0 state)))
                 :string
                 (pop-item :string (pop-item :integer state)))
      state)))

(define-registered
  string_substring
  ^{:stack-types [:string :integer]}
  (fn [state]
    (if (and (not (empty? (:string state)))
             (not (empty? (rest (:integer state)))))
      (let [st (stack-ref :string 0 state)
            first-index (min (count st) (max 0 (stack-ref :integer 1 state)))
            second-index (min (count st) (max first-index (stack-ref :integer 0 state)))]
        (push-item (subs st first-index second-index)
                   :string
                   (pop-item :string (pop-item :integer (pop-item :integer state)))))
      state)))

(define-registered
  string_first
  ^{:stack-types [:string :char]}
  (fn [state]
    (if (and (not (empty? (:string state)))
             (first (first (:string state))))
      (push-item (first (stack-ref :string 0 state))
                 :char
                 (pop-item :string state))
      state)))

(define-registered
  string_last
  ^{:stack-types [:string :char]}
  (fn [state]
    (if (and (not (empty? (:string state)))
             (last (first (:string state))))
      (push-item (last (stack-ref :string 0 state))
                 :char
                 (pop-item :string state))
      state)))

(define-registered
  string_nth
  ^{:stack-types [:string :char :integer]}
  (fn [state]
    (if (and (not (empty? (:string state)))
             (not (empty? (:integer state)))
             (not (empty? (first (:string state)))))
      (let [st (stack-ref :string 0 state)
            index (mod (stack-ref :integer 0 state) (count st))]
        (push-item (nth st index)
                   :char
                   (pop-item :integer (pop-item :string state))))
      state)))

(define-registered
  string_rest
  ^{:stack-types [:string]}
  (fn [state]
    (if (not (empty? (:string state)))
      (push-item (apply str (rest (stack-ref :string 0 state)))
                 :string
                 (pop-item :string state))
      state)))

(define-registered
  string_butlast
  ^{:stack-types [:string]}
  (fn [state]
    (if (not (empty? (:string state)))
      (push-item (apply str (butlast (stack-ref :string 0 state)))
                 :string
                 (pop-item :string state))
      state)))

(define-registered
  string_length
  ^{:stack-types [:string :integer]}
  (fn [state]
    (if (not (empty? (:string state)))
      (push-item (count (stack-ref :string 0 state))
                 :integer
                 (pop-item :string state))
      state)))

(define-registered
  string_reverse
  ^{:stack-types [:string]}
  (fn [state]
    (if (empty? (:string state))
      state
      (let [top-string (top-item :string state)]
        (push-item (apply str (reverse top-string))
                   :string
                   (pop-item :string state))))))

(define-registered
  string_parse_to_chars
  ^{:stack-types [:string]}
  (fn [state]
    (if (empty? (:string state))
      state
      (loop [char-list (reverse (top-item :string state))
             loop-state (pop-item :string state)]
        (if (empty? char-list)
          loop-state
          (recur (rest char-list)
                 (push-item (str (first char-list)) :string loop-state)))))))

(define-registered
  string_split
  ^{:stack-types [:string]}
  (fn [state]
    (if (empty? (:string state))
      state
      (loop [word-list (reverse (filter not-empty (split (trim (top-item :string state)) #"\s+")))
             loop-state (pop-item :string state)]
        (if (empty? word-list)
          loop-state
          (recur (rest word-list)
                 (push-item (first word-list) :string loop-state)))))))

(define-registered
  string_emptystring ;;true if top string is empty
  ^{:stack-types [:string :boolean]}
  (fn [state]
    (if (empty? (:string state))
      state
      (let [result-boolean (empty? (top-item :string state))]
        (push-item result-boolean
                   :boolean
                   (pop-item :string state))))))

(define-registered
  string_contains ;;true if top string is a substring of second string; false otherwise
  ^{:stack-types [:string :boolean]}
  (fn [state]
    (if (empty? (rest (:string state)))
      state
      (let [sub (top-item :string state)
            full (stack-ref :string 1 state)
            result-boolean (if (<= 0 (.indexOf full sub))
                             true
                             false)]
        (push-item result-boolean
                   :boolean
                   (pop-item :string (pop-item :string state)))))))

(define-registered
  string_containschar ; true if the top char is in the top string
  ^{:stack-types [:string :boolean :char]}
  (fn [state]
    (if (or (empty? (:string state))
            (empty? (:char state)))
      state
      (let [sub (str (top-item :char state))
            full (stack-ref :string 0 state)
            result (<= 0 (.indexOf full sub))]
        (push-item result
                   :boolean
                   (pop-item :char (pop-item :string state)))))))

(define-registered
  string_indexofchar ; puts on the integer stack the index of the top char in the top string
  ^{:stack-types [:string :integer :char]}
  (fn [state]
    (if (or (empty? (:string state))
            (empty? (:char state)))
      state
      (let [sub (str (top-item :char state))
            full (stack-ref :string 0 state)
            index (.indexOf full sub)]
        (push-item index
                   :integer
                   (pop-item :char (pop-item :string state)))))))

(define-registered
  string_occurrencesofchar ; the number of times the top char is in the top string
  ^{:stack-types [:string :integer :char]}
  (fn [state]
    (if (or (empty? (:string state))
            (empty? (:char state)))
      state
      (let [ch (stack-ref :char 0 state)
            st (stack-ref :string 0 state)
            occ (count (filter #{ch} st))]
        (push-item occ
                   :integer
                   (pop-item :char (pop-item :string state)))))))

(define-registered
  string_replace ; In third string on stack, replaces all occurences of second string with first string
  ^{:stack-types [:string]}
  (fn [state]
    (if (<= 3 (count (:string state)))
      (let [result (string/replace (stack-ref :string 2 state)
                                   (stack-ref :string 1 state)
                                   (stack-ref :string 0 state))]
        (if (>= max-string-length (count result))
          (push-item result
                     :string
                     (pop-item :string (pop-item :string (pop-item :string state))))
          state))
      state)))

(define-registered
  string_replacefirst ; In third string on stack, replaces first occurence of second string with first string
  ^{:stack-types [:string]}
  (fn [state]
    (if (<= 3 (count (:string state)))
      (let [result (string/replace-first (stack-ref :string 2 state)
                                         (stack-ref :string 1 state)
                                         (stack-ref :string 0 state))]
        (if (>= max-string-length (count result))
          (push-item result
                     :string
                     (pop-item :string (pop-item :string (pop-item :string state))))
          state))
      state)))

(define-registered
  string_replacechar ; In top string on stack, replaces all occurences of second char with first char
  ^{:stack-types [:string :char]}
  (fn [state]
    (if (and (not (empty? (:string state)))
             (<= 2 (count (:char state))))
      (let [result (string/replace (stack-ref :string 0 state)
                                   (stack-ref :char 1 state)
                                   (stack-ref :char 0 state))]
        (if (>= max-string-length (count result))
          (push-item result
                     :string
                     (pop-item :char (pop-item :char (pop-item :string state))))
          state))
      state)))

(define-registered
  string_replacefirstchar ; In top string on stack, replaces first occurence of second char with first char
  ^{:stack-types [:string :char]}
  (fn [state]
    (if (and (not (empty? (:string state)))
             (<= 2 (count (:char state))))
      (let [result (string/replace-first (stack-ref :string 0 state)
                                   (stack-ref :char 1 state)
                                   (stack-ref :char 0 state))]
        (if (>= max-string-length (count result))
          (push-item result
                     :string
                     (pop-item :char (pop-item :char (pop-item :string state))))
          state))
      state)))

(define-registered
  string_removechar ; In top string on stack, remove all occurences of char
  ^{:stack-types [:string :char]}
  (fn [state]
    (if (and (not (empty? (:string state)))
             (not (empty? (:char state))))
      (let [result (apply str (remove #{(stack-ref :char 0 state)}
                                      (stack-ref :string 0 state)))]
        (push-item result
                   :string
                   (pop-item :char (pop-item :string state))))
      state)))

  (define-registered
  string_setchar ; Returns a function that sets char at index in string
  ^{:stack-types [:string :char :integer]}
  (fn [state]
    (if (or (empty? (:string state))
            (empty? (:char state))
            (empty? (:integer state)))
      state
      (let [s (top-item :string state)
            item (top-item :char state)
            index (if (empty? s)
                    0
                    (mod (top-item :integer state) (count s)))
            result (if (empty? s)
                     s
                     (apply str (assoc (vec s) index item)))]
        (push-item result
                   :string
                   (pop-item :char (pop-item :integer (pop-item :string state))))))))

(define-registered
  exec_string_iterate ; Returns a function that iterates over a string using the code on the exec stack.
  ^{:stack-types [:string :char :exec] :parentheses 1}
  (fn [state]
    (if (or (empty? (:string state))
            (empty? (:exec state)))
      state
      (let [s (top-item :string state)]
      (cond
        (empty? s) (->> state
                        (pop-item :string)
                        (pop-item :exec))
        (empty? (rest s)) (->> state ;If the rest of the string is empty, we're done iterating.
                            (pop-item :string)
                            (push-item (first s) :char))
        :else (->> state
                (pop-item :string)
                (push-item 'exec_string_iterate :exec)
                (push-item (apply str (rest s)) :exec)
                (push-item (top-item :exec state) :exec)
                (push-item (first s) :char)))))))
