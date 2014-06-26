(ns clojush.instructions.common
  (:use [clojush pushstate globals]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lookup function to see how many paren groups a function requires

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lookup function to see how many paren groups a function requires

(def instr-paren-requirements
  (atom {;; Require 3
         'exec_rot 3
         'exec_s 3
         ;; Require 2
         'exec_if 2
         'exec_swap 2
         'exec_k 2
         ;; Require 1
         'exec_when 1
         'exec_pop 1
         'exec_dup 1
         'exec_shove 1
         'exec_do*range 1
         'exec_do*count 1
         'exec_do*times 1
         'exec_y 1
         'return_fromexec 1
         'environment_new 1
         'zip_fromexec 1
         'zip_replace_fromexec 1
         'zip_insert_right_fromexec 1
         'zip_insert_left_fromexec 1
         'zip_insert_child_fromexec 1
         'zip_append_child_fromexec 1
         ;; Require 0, but included here in case change mind later
         ;; (default for not-mentioned instructions is 0)
         'exec_eq 0
         'exec_yank 0
         'exec_yankdup 0
         }))

(defn lookup-instruction-paren-groups
  [ins]
  (let [ins-req (get @instr-paren-requirements ins)]
  (cond
    ins-req ins-req
    (and (symbol? ins)
         (.startsWith (name ins) "tag_exec")) 1
    :else 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; instructions for all types (except auxiliary and tag)

(defn popper 
  "Returns a function that takes a state and pops the appropriate stack of the state."
  [type]
  (fn [state] (pop-item type state)))

(define-registered exec_pop (popper :exec))
(define-registered integer_pop (popper :integer))
(define-registered float_pop (popper :float))
(define-registered code_pop (popper :code))
(define-registered boolean_pop (popper :boolean))
(define-registered zip_pop (popper :zip))
(define-registered string_pop (popper :string))

(defn duper 
  "Returns a function that takes a state and duplicates the top item of the appropriate 
   stack of the state."
  [type]
  (fn [state]
    (if (empty? (type state))
      state
      (push-item (top-item type state) type state))))

(define-registered exec_dup (duper :exec))
(define-registered integer_dup (duper :integer))
(define-registered float_dup (duper :float))
(define-registered code_dup (duper :code))
(define-registered boolean_dup (duper :boolean))
(define-registered zip_dup (duper :zip))
(define-registered string_dup (duper :string))

(defn swapper 
  "Returns a function that takes a state and swaps the top 2 items of the appropriate 
   stack of the state."
  [type]
  (fn [state]
    (if (not (empty? (rest (type state))))
      (let [first-item (stack-ref type 0 state)
            second-item (stack-ref type 1 state)]
        (->> (pop-item type state) 
             (pop-item type)
             (push-item first-item type)
             (push-item second-item type)))
      state)))

(define-registered exec_swap (swapper :exec))
(define-registered integer_swap (swapper :integer))
(define-registered float_swap (swapper :float))
(define-registered code_swap (swapper :code))
(define-registered boolean_swap (swapper :boolean))
(define-registered zip_swap (swapper :zip))
(define-registered string_swap (swapper :string))

(defn rotter 
  "Returns a function that takes a state and rotates the top 3 items of the appropriate 
   stack of the state."
  [type]
  (fn [state]
    (if (not (empty? (rest (rest (type state)))))
      (let [first (stack-ref type 0 state)
            second (stack-ref type 1 state)
            third (stack-ref type 2 state)]
        (->> (pop-item type state)
             (pop-item type)
             (pop-item type)
             (push-item second type)
             (push-item first type)
             (push-item third type)))
      state)))

(define-registered exec_rot (rotter :exec))
(define-registered integer_rot (rotter :integer))
(define-registered float_rot (rotter :float))
(define-registered code_rot (rotter :code))
(define-registered boolean_rot (rotter :boolean))
(define-registered zip_rot (rotter :zip))
(define-registered string_rot (rotter :string))

(defn flusher
  "Returns a function that empties the stack of the given state."
  [type]
  (fn [state]
    (assoc state type '())))

(define-registered exec_flush (flusher :exec))
(define-registered integer_flush (flusher :integer))
(define-registered float_flush (flusher :float))
(define-registered code_flush (flusher :code))
(define-registered boolean_flush (flusher :boolean))
(define-registered zip_flush (flusher :zip))
(define-registered string_flush (flusher :string))


(defn eqer 
  "Returns a function that compares the top two items of the appropriate stack of 
   the given state."
  [type]
  (fn [state]
    (if (not (empty? (rest (type state))))
      (let [first (stack-ref type 0 state)
            second (stack-ref type 1 state)]
        (->> (pop-item type state)
             (pop-item type)
             (push-item (= first second) :boolean)))
      state)))

(define-registered exec_eq (eqer :exec))
(define-registered integer_eq (eqer :integer))
(define-registered float_eq (eqer :float))
(define-registered code_eq (eqer :code))
(define-registered boolean_eq (eqer :boolean))
(define-registered zip_eq (eqer :zip))
(define-registered string_eq (eqer :string))

(defn stackdepther
  "Returns a function that pushes the depth of the appropriate stack of the 
   given state."
  [type]
  (fn [state]
    (push-item (count (type state)) :integer state)))

(define-registered exec_stackdepth (stackdepther :exec))
(define-registered integer_stackdepth (stackdepther :integer))
(define-registered float_stackdepth (stackdepther :float))
(define-registered code_stackdepth (stackdepther :code))
(define-registered boolean_stackdepth (stackdepther :boolean))
(define-registered zip_stackdepth (stackdepther :zip))
(define-registered string_stackdepth (stackdepther :string))

(defn yanker
  "Returns a function that yanks an item from deep in the specified stack,
   using the top integer to indicate how deep."
  [type]
  (fn [state]
    (if (or (and (= type :integer)
                 (not (empty? (rest (type state)))))
            (and (not (= type :integer))
                 (not (empty? (type state)))
                 (not (empty? (:integer state)))))
      (let [raw-index (stack-ref :integer 0 state)
            with-index-popped (pop-item :integer state)
            actual-index (max 0 (min raw-index (- (count (type with-index-popped)) 1)))
            item (stack-ref type actual-index with-index-popped)
            with-item-pulled (assoc with-index-popped 
                                    type 
                                    (let [stk (type with-index-popped)]
                                      (concat (take actual-index stk)
                                              (rest (drop actual-index stk)))))]
        (push-item item type with-item-pulled))
      state)))

(define-registered exec_yank (yanker :exec))
(define-registered integer_yank (yanker :integer))
(define-registered float_yank (yanker :float))
(define-registered code_yank (yanker :code))
(define-registered boolean_yank (yanker :boolean))
(define-registered zip_yank (yanker :zip))
(define-registered string_yank (yanker :string))

(defn yankduper
  "Returns a function that yanks a copy of an item from deep in the specified stack,
   using the top integer to indicate how deep."
  [type]
  (fn [state]
    (if (or (and (= type :integer)
                 (not (empty? (rest (type state)))))
            (and (not (= type :integer))
                 (not (empty? (type state)))
                 (not (empty? (:integer state)))))
      (let [raw-index (stack-ref :integer 0 state)
            with-index-popped (pop-item :integer state)
            actual-index (max 0 (min raw-index (- (count (type with-index-popped)) 1)))
            item (stack-ref type actual-index with-index-popped)]
        (push-item item type with-index-popped))
      state)))

(define-registered exec_yankdup (yankduper :exec))
(define-registered integer_yankdup (yankduper :integer))
(define-registered float_yankdup (yankduper :float))
(define-registered code_yankdup (yankduper :code))
(define-registered boolean_yankdup (yankduper :boolean))
(define-registered zip_yankdup (yankduper :zip))
(define-registered string_yankdup (yankduper :string))

(defn shover
  "Returns a function that shoves an item deep in the specified stack, using the top
   integer to indicate how deep."
  [type]
  (fn [state]
    (if (or (and (= type :integer)
                 (not (empty? (rest (type state)))))
            (and (not (= type :integer))
                 (not (empty? (type state)))
                 (not (empty? (:integer state)))))
      (let [raw-index (stack-ref :integer 0 state)
            with-index-popped (pop-item :integer state)
            item (top-item type with-index-popped)
            with-args-popped (pop-item type with-index-popped)
            actual-index (max 0 (min raw-index (count (type with-args-popped))))]
        (assoc with-args-popped type (let [stk (type with-args-popped)]
                                       (concat (take actual-index stk)
                                               (list item)
                                               (drop actual-index stk)))))
      state)))

(define-registered exec_shove (shover :exec))
(define-registered integer_shove (shover :integer))
(define-registered float_shove (shover :float))
(define-registered code_shove (shover :code))
(define-registered boolean_shove (shover :boolean))
(define-registered zip_shove (shover :zip))
(define-registered string_shove (shover :string))
