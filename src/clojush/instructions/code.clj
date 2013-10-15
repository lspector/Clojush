(ns clojush.instructions.code
  (:use [clojush.pushstate]
        [clojush.util]
        [clojush.globals])
  (:require [clojure.math.numeric-tower :as math]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; code and exec instructions

(define-registered 
  code_append
  (fn [state]
    (if (not (empty? (rest (:code state))))
      (let [new-item (concat (ensure-list (stack-ref :code 0 state))
                             (ensure-list (stack-ref :code 1 state)))]
        (if (<= (count-points new-item) @global-max-points)
          (push-item new-item
                     :code
                     (pop-item :code (pop-item :code state)))
          state))
      state)))

(define-registered 
  code_atom
  (fn [state]
    (if (not (empty? (:code state)))
      (push-item (not (seq? (stack-ref :code 0 state)))
                 :boolean
                 (pop-item :code state))
      state)))

(define-registered 
  code_car
  (fn [state]
    (if (and (not (empty? (:code state)))
             (> (count (ensure-list (stack-ref :code 0 state))) 0))
      (push-item (first (ensure-list (stack-ref :code 0 state)))
                 :code
                 (pop-item :code state))
      state)))

(define-registered 
  code_cdr
  (fn [state]
    (if (not (empty? (:code state)))
      (push-item (rest (ensure-list (stack-ref :code 0 state)))
                 :code
                 (pop-item :code state))
      state)))

(define-registered 
  code_cons
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
  (fn [state]
    (if (not (empty? (:code state)))
      (push-item (stack-ref :code 0 state) 
                 :exec
                 (push-item 'code_pop :exec state))
      state)))

(define-registered 
  code_do*
  (fn [state]
    (if (not (empty? (:code state)))
      (push-item (stack-ref :code 0 state)
                 :exec
                 (pop-item :code state))
      state)))

(define-registered 
  code_do*range
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
  code_map
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

(define-registered code_fromboolean (codemaker :boolean))
(define-registered code_fromfloat (codemaker :float))
(define-registered code_frominteger (codemaker :integer))
(define-registered code_quote (codemaker :exec))

(define-registered 
  code_if
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
  (fn [state]
    (if (not (or (empty? (:boolean state))
                 (empty? (:exec state))))
      (if (first (:boolean state))
        (pop-item :boolean state)
        (pop-item :boolean (pop-item :exec state)))
      state)))

(define-registered 
  code_length
  (fn [state]
    (if (not (empty? (:code state)))
      (push-item (count (ensure-list (first (:code state))))
                 :integer
                 (pop-item :code state))
      state)))

(define-registered 
  code_list
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
  (fn [state]
    (if (not (empty? (rest (:code state))))
      (push-item (not (not (some #{(first (rest (:code state)))} 
                                 (ensure-list (first (:code state))))))
                 :boolean
                 (pop-item :code (pop-item :code state)))
      state)))

(define-registered exec_noop (fn [state] state))
(define-registered code_noop (fn [state] state))

(define-registered 
  code_nth
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
  (fn [state]
    (if (not (empty? (:code state)))
      (push-item (let [item (first (:code state))]
                   (not (not (and (seq? item) (empty? item)))))
                 :boolean
                 (pop-item :code state))
      state)))

(define-registered 
  code_size
  (fn [state]
    (if (not (empty? (:code state)))
      (push-item (count-points (first (:code state)))
                 :integer
                 (pop-item :code state))
      state))) 

(define-registered 
  code_extract
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
  (fn [state]
    (if (not (empty? (rest (:code state))))
      (push-item (contains-subtree (stack-ref :code 1 state)
                                   (stack-ref :code 0 state))
                 :boolean
                 (pop-item :code (pop-item :code state)))
      state)))

(define-registered 
  code_container
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
  (fn [state]
    (if (not (empty? (rest (:exec state))))
      (push-item (first (:exec state))
                 :exec
                 (pop-item :exec (pop-item :exec state)))
      state)))

(define-registered 
  exec_s
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

(define-registered
  environment_new
  (fn [state]
    (if (empty? (:exec state))
      state
      (let [new-exec (top-item :exec state)
            parent-env (pop-item :exec state)]
        (push-item new-exec
                   :exec
                   (assoc (assoc (push-item parent-env :environment state)
                                 :return '())
                          :exec '()))))))

(define-registered
  environment_begin
  (fn [state]
    (assoc (push-item (assoc state :exec '())
                      :environment state)
           :return '())))

(define-registered
  environment_end
  (fn [state]
    (if (empty? (:environment state))
      state
      (end-environment state))))
