(ns clojush.instructions.code
  (:use [clojush.pushstate]
        [clojush.util]
        [clojush.globals])
  (:require [clojure.math.numeric-tower :as math]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; code and exec instructions

(define-registered exec_noop ^{:stack-types [:exec]} (fn [state] state))
(define-registered code_noop ^{:stack-types [:code]} (fn [state] state))

(define-registered noop_open_paren ^{:stack-types [:parentheses] :parentheses 1} (fn [state] state))
(define-registered noop_delete_prev_paren_pair ^{:stack-types [:parentheses] :parentheses 0} (fn [state] state))

(define-registered 
  code_append
  ^{:stack-types [:code]}
  (fn [state]
    (if (not (empty? (rest (:code state))))
      (let [new-item (not-lazy
                       (concat (ensure-list (stack-ref :code 0 state))
                               (ensure-list (stack-ref :code 1 state))))]
        (if (<= (count-points new-item) @global-max-points)
          (push-item new-item
                     :code
                     (pop-item :code (pop-item :code state)))
          state))
      state)))

(define-registered 
  code_atom
  ^{:stack-types [:code :boolean]}
  (fn [state]
    (if (not (empty? (:code state)))
      (push-item (not (seq? (stack-ref :code 0 state)))
                 :boolean
                 (pop-item :code state))
      state)))

(define-registered 
  code_car
  ^{:stack-types [:code]}
  (fn [state]
    (if (and (not (empty? (:code state)))
             (> (count (ensure-list (stack-ref :code 0 state))) 0))
      (push-item (first (ensure-list (stack-ref :code 0 state)))
                 :code
                 (pop-item :code state))
      state)))

(define-registered 
  code_cdr
  ^{:stack-types [:code]}
  (fn [state]
    (if (not (empty? (:code state)))
      (push-item (rest (ensure-list (stack-ref :code 0 state)))
                 :code
                 (pop-item :code state))
      state)))

(define-registered 
  code_cons
  ^{:stack-types [:code]}
  (fn [state]
    (if (not (empty? (rest (:code state))))
      (let [new-item (cons (stack-ref :code 1 state)
                           (ensure-list (stack-ref :code 0 state)))]
        (if (<= (count-points new-item) @global-max-points)
          (push-item new-item
                     :code
                     (pop-item :code (pop-item :code state)))
          state))
      state)))

(define-registered 
  code_do
  ^{:stack-types [:code :exec]}
  (fn [state]
    (if (not (empty? (:code state)))
      (push-item (stack-ref :code 0 state) 
                 :exec
                 (push-item 'code_pop :exec state))
      state)))

(define-registered 
  code_do*
  ^{:stack-types [:code :exec]}
  (fn [state]
    (if (not (empty? (:code state)))
      (push-item (stack-ref :code 0 state)
                 :exec
                 (pop-item :code state))
      state)))

(define-registered 
  code_do*range
  ^{:stack-types [:code :exec :integer]}
  (fn [state]
    (if (not (or (empty? (:code state))
                 (empty? (rest (:integer state)))))
      (let [to-do (first (:code state))
            current-index (first (rest (:integer state)))
            destination-index (first (:integer state))
            args-popped (pop-item :integer
                                  (pop-item :integer
                                            (pop-item :code state)))
            increment (cond (< current-index destination-index) 1
                            (> current-index destination-index) -1
                            true 0)
            continuation (if (zero? increment)
                           args-popped
                           (push-item (list (+' current-index increment)
                                            destination-index
                                            'code_quote
                                            to-do
                                            'code_do*range)
                                      :exec
                                      args-popped))]
        (push-item to-do :exec (push-item current-index :integer continuation)))
      state)))

(define-registered 
  exec_do*range
  ^{:stack-types [:exec :integer]
    :parentheses 1}
  (fn [state] ; Differs from code.do*range only in the source of the code and the recursive call.
    (if (not (or (empty? (:exec state))
                 (empty? (rest (:integer state)))))
      (let [to-do (first (:exec state))
            current-index (first (rest (:integer state)))
            destination-index (first (:integer state))
            args-popped (pop-item :integer
                                  (pop-item :integer
                                            (pop-item :exec state)))
            increment (cond (< current-index destination-index) 1
                            (> current-index destination-index) -1
                            true 0)
            continuation (if (zero? increment)
                           args-popped
                           (push-item (list (+' current-index increment)
                                            destination-index
                                            'exec_do*range
                                            to-do)
                                      :exec
                                      args-popped))]
        (push-item to-do :exec (push-item current-index :integer continuation)))
      state)))

(define-registered 
  code_do*count
  ^{:stack-types [:code :exec :integer]}
  (fn [state]
    (if (not (or (empty? (:integer state))
                 (< (first (:integer state)) 1)
                 (empty? (:code state))))
      (push-item (list 0 (dec (first (:integer state))) 
                       'code_quote (first (:code state)) 'code_do*range)
                 :exec
                 (pop-item :integer (pop-item :code state)))
      state)))

(define-registered
  exec_do*count
  ^{:stack-types [:exec :integer]
    :parentheses 1}
  ;; differs from code.do*count only in the source of the code and the recursive call    
  (fn [state] 
    (if (not (or (empty? (:integer state))
                 (< (first (:integer state)) 1)
                 (empty? (:exec state))))
      (push-item (list 0 (dec (first (:integer state))) 'exec_do*range (first (:exec state)))
                 :exec
                 (pop-item :integer (pop-item :exec state)))
      state)))

(define-registered
  code_do*times
  ^{:stack-types [:code :exec :integer]}
  (fn [state]
    (if (not (or (empty? (:integer state))
                 (< (first (:integer state)) 1)
                 (empty? (:code state))))
      (push-item (list 0 (dec (first (:integer state))) 'code_quote 
                       (cons 'integer_pop 
                             (ensure-list (first (:code state)))) 'code_do*range)
                 :exec
                 (pop-item :integer (pop-item :code state)))
      state)))

(define-registered
  exec_do*times
  ^{:stack-types [:exec :integer]
    :parentheses 1}
  ;; differs from code.do*times only in the source of the code and the recursive call
  (fn [state]
    (if (not (or (empty? (:integer state))
                 (< (first (:integer state)) 1)
                 (empty? (:exec state))))
      (push-item (list 0 (dec (first (:integer state))) 'exec_do*range
                       (cons 'integer_pop (ensure-list (first (:exec state)))))
                 :exec
                 (pop-item :integer (pop-item :exec state)))
      state)))

(define-registered
  exec_while
  ^{:stack-types [:exec :boolean]
    :parentheses 1}
  (fn [state]
    (if (empty? (:exec state))
      state
      (if (empty? (:boolean state))
        (pop-item :exec state)
        (if (not (stack-ref :boolean 0 state))
          (pop-item :exec (pop-item :boolean state))
          (let [block (stack-ref :exec 0 state)]
            (pop-item :boolean (push-item block :exec (push-item 'exec_while :exec state)))))))))

(define-registered
  exec_do*while
  ^{:stack-types [:exec :boolean]
    :parentheses 1}
  (fn [state]
    (if (empty? (:exec state))
      state
      (let [block (stack-ref :exec 0 state)]
        (push-item block :exec (push-item 'exec_while :exec state))))))

(define-registered 
  code_map
  ^{:stack-types [:code :exec]}
  (fn [state]
    (if (not (or (empty? (:code state))
                 (empty? (:exec state))))
      (push-item (concat
                   (doall (for [item (ensure-list (first (:code state)))]
                            (list 'code_quote
                                  item
                                  (first (:exec state)))))
                   '(code_wrap)
                   (doall (for [item (rest (ensure-list (first (:code state))))]
                            'code_cons)))
                 :exec
                 (pop-item :code (pop-item :exec state)))
      state)))

(defn codemaker
  "Returns a function that pops the stack of the given type and pushes the result on 
   the code stack."
  [type]
  (fn [state]
    (if (not (empty? (type state)))
      (push-item (first (type state))
                 :code
                 (pop-item type state))
      state)))

(define-registered code_fromboolean (with-meta (codemaker :boolean) {:stack-types [:code :boolean]}))
(define-registered code_fromfloat (with-meta (codemaker :float) {:stack-types [:code :float]}))
(define-registered code_frominteger (with-meta (codemaker :integer) {:stack-types [:code :integer]}))
(define-registered code_quote (with-meta (codemaker :exec) {:stack-types [:code :exec] :parentheses 1}))

(define-registered 
  code_if
  ^{:stack-types [:code :exec :boolean]}
  (fn [state]
    (if (not (or (empty? (:boolean state))
                 (empty? (rest (:code state)))))
      (push-item (if (first (:boolean state))
                   (first (rest (:code state)))
                   (first (:code state)))
                 :exec
                 (pop-item :boolean (pop-item :code (pop-item :code state))))
      state)))

(define-registered 
  exec_if
  ^{:stack-types [:exec :boolean]
    :parentheses 2}
  ;; differs from code.if in the source of the code and in the order of the if/then parts
  (fn [state]
    (if (not (or (empty? (:boolean state))
                 (empty? (rest (:exec state)))))
      (push-item (if (first (:boolean state))
                   (first (:exec state))
                   (first (rest (:exec state))))
                 :exec
                 (pop-item :boolean (pop-item :exec (pop-item :exec state))))
      state)))

(define-registered 
  exec_when
  ^{:stack-types [:exec :boolean]
    :parentheses 1}
  (fn [state]
    (if (not (or (empty? (:boolean state))
                 (empty? (:exec state))))
      (if (first (:boolean state))
        (pop-item :boolean state)
        (pop-item :boolean (pop-item :exec state)))
      state)))

(define-registered 
  code_length
  ^{:stack-types [:code :integer]}
  (fn [state]
    (if (not (empty? (:code state)))
      (push-item (count (ensure-list (first (:code state))))
                 :integer
                 (pop-item :code state))
      state)))

(define-registered 
  code_list
  ^{:stack-types [:code]}
  (fn [state]
    (if (not (empty? (rest (:code state))))
      (let [new-item (list (first (rest (:code state)))
                           (first (:code state)))]
        (if (<= (count-points new-item) @global-max-points)
          (push-item new-item
                     :code
                     (pop-item :code (pop-item :code state)))
          state))
      state)))

(define-registered 
  code_wrap
  ^{:stack-types [:code]}
  (fn [state]
    (if (not (empty? (:code state)))
      (let [new-item (list (first (:code state)))]
        (if (<= (count-points new-item) @global-max-points)
          (push-item new-item
                     :code
                     (pop-item :code state))
          state))
      state)))

(define-registered 
  code_member
  ^{:stack-types [:code :boolean]}
  (fn [state]
    (if (not (empty? (rest (:code state))))
      (push-item (not (not (some #{(first (rest (:code state)))} 
                                 (ensure-list (first (:code state))))))
                 :boolean
                 (pop-item :code (pop-item :code state)))
      state)))

(define-registered 
  code_nth
  ^{:stack-types [:code :integer]}
  (fn [state]
    (if (not (or (empty? (:integer state))
                 (empty? (:code state))
                 (empty? (ensure-list (first (:code state))))))
      (push-item (nth (ensure-list (first (:code state)))
                      (mod (math/abs (first (:integer state)))
                           (count (ensure-list (first (:code state))))))
                 :code
                 (pop-item :integer (pop-item :code state)))
      state)))

(define-registered 
  code_nthcdr
  ^{:stack-types [:code :integer]}
  (fn [state]
    (if (not (or (empty? (:integer state))
                 (empty? (:code state))
                 (empty? (ensure-list (first (:code state))))))
      (push-item (drop (mod (math/abs (first (:integer state))) 
                            (count (ensure-list (first (:code state)))))
                       (ensure-list (first (:code state))))
                 :code
                 (pop-item :integer (pop-item :code state)))
      state)))

(define-registered 
  code_null
  ^{:stack-types [:code :boolean]}
  (fn [state]
    (if (not (empty? (:code state)))
      (push-item (let [item (first (:code state))]
                   (not (not (and (seq? item) (empty? item)))))
                 :boolean
                 (pop-item :code state))
      state)))

(define-registered 
  code_size
  ^{:stack-types [:code :integer]}
  (fn [state]
    (if (not (empty? (:code state)))
      (push-item (count-points (first (:code state)))
                 :integer
                 (pop-item :code state))
      state))) 

(define-registered 
  code_extract
  ^{:stack-types [:code :integer]}
  (fn [state]
    (if (not (or (empty? (:code state))
                 (empty? (:integer state))))
      (push-item (code-at-point (first (:code state))
                                (first (:integer state)))
                 :code
                 (pop-item :code (pop-item :integer state)))
      state)))

(define-registered 
  code_insert
  ^{:stack-types [:code :integer]}
  (fn [state]
    (if (not (or (empty? (rest (:code state)))
                 (empty? (:integer state))))
      (let [new-item (insert-code-at-point (first (:code state))
                                           (first (:integer state))
                                           (second (:code state)))]
        (if (<= (count-points new-item) @global-max-points)
          (push-item new-item
                     :code
                     (pop-item :code (pop-item :code (pop-item :integer state))))
          state))
      state)))

(define-registered 
  code_subst
  ^{:stack-types [:code]}
  (fn [state]
    (if (not (empty? (rest (rest (:code state)))))
      (let [new-item (subst (stack-ref :code 2 state)
                            (stack-ref :code 1 state)
                            (stack-ref :code 0 state))]
        (if (<= (count-points new-item) @global-max-points)
          (push-item new-item
                     :code
                     (pop-item :code (pop-item :code (pop-item :code state))))
          state))
      state)))

(define-registered 
  code_contains
  ^{:stack-types [:code :boolean]}
  (fn [state]
    (if (not (empty? (rest (:code state))))
      (push-item (contains-subtree (stack-ref :code 1 state)
                                   (stack-ref :code 0 state))
                 :boolean
                 (pop-item :code (pop-item :code state)))
      state)))

(define-registered 
  code_container
  ^{:stack-types [:code]}
  (fn [state]
    (if (not (empty? (rest (:code state))))
      (push-item (containing-subtree (stack-ref :code 0 state)
                                     (stack-ref :code 1 state))
                 :code
                 (pop-item :code (pop-item :code state)))
      state)))

;; clojure.contrib/positions disappeared from libraries, but this is the function
;;   with the old indexed function expanded.
(defn positions
  "Returns a lazy sequence containing the positions at which pred
   is true for items in coll."
  [pred coll]
  (for [[idx elt] (map vector (iterate inc 0) coll) :when (pred elt)] idx))

(define-registered 
  code_position
  ^{:stack-types [:code :integer]}
  (fn [state]
    (if (not (empty? (rest (:code state))))
      (push-item (or (first (positions #{(stack-ref :code 1 state)}
                                       (ensure-list (stack-ref :code 0 state))))
                     -1)
                 :integer
                 (pop-item :code (pop-item :code state)))
      state)))

(define-registered 
  exec_k
  ^{:stack-types [:exec]
    :parentheses 2}
  (fn [state]
    (if (not (empty? (rest (:exec state))))
      (push-item (first (:exec state))
                 :exec
                 (pop-item :exec (pop-item :exec state)))
      state)))

(define-registered 
  exec_s
  ^{:stack-types [:exec]
    :parentheses 3}
  (fn [state]
    (if (not (empty? (rest (rest (:exec state)))))
      (let [stk (:exec state)
            x (first stk)
            y (first (rest stk))
            z (first (rest (rest stk)))]
        (if (<= (count-points (list y z)) @global-max-points)
          (push-item x
                     :exec
                     (push-item z
                                :exec
                                (push-item (list y z)
                                           :exec
                                           (pop-item :exec 
                                                     (pop-item :exec 
                                                               (pop-item :exec state))))))
          state))
      state)))

(define-registered 
  exec_y
  ^{:stack-types [:exec]
    :parentheses 1}
  (fn [state]
    (if (not (empty? (:exec state)))
      (let [new-item (list 'exec_y (first (:exec state)))]
        (if (<= (count-points new-item) @global-max-points)
          (push-item (first (:exec state))
                     :exec
                     (push-item new-item
                                :exec
                                (pop-item :exec state)))
          state))
      state)))

