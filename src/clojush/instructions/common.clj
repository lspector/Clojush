(ns clojush.instructions.common
  (:use [clojush pushstate globals util]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lookup function to see how many paren groups a function requires. Uses metadata.

(defn lookup-instruction-paren-groups
  [ins]
  (let [ins-req (:parentheses (meta (get @instruction-table ins)))]
    (cond
      ins-req ins-req
      (and (symbol? ins)
           (.startsWith (name ins) "tag_exec")) 1
      :else 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; instructions for all types (except non-data stacks such as auxiliary, tag, input, and output)

(defn popper
  "Returns a function that takes a state and pops the appropriate stack of the state."
  [type]
  (fn [state] (pop-item type state)))

(define-registered exec_pop (with-meta (popper :exec) {:stack-types [:exec] :parentheses 1}))
(define-registered integer_pop (with-meta (popper :integer) {:stack-types [:integer]}))
(define-registered float_pop (with-meta (popper :float) {:stack-types [:float]}))
(define-registered code_pop (with-meta (popper :code) {:stack-types [:code]}))
(define-registered boolean_pop (with-meta (popper :boolean) {:stack-types [:boolean]}))
(define-registered zip_pop (with-meta (popper :zip) {:stack-types [:zip]}))
(define-registered string_pop (with-meta (popper :string) {:stack-types [:string]}))
(define-registered char_pop (with-meta (popper :char) {:stack-types [:char]}))

(defn duper
  "Returns a function that takes a state and duplicates the top item of the appropriate 
   stack of the state."
  [type]
  (fn [state]
    (if (empty? (type state))
      state
      (push-item (top-item type state) type state))))

(define-registered exec_dup (with-meta (duper :exec) {:stack-types [:exec] :parentheses 1}))
(define-registered integer_dup (with-meta (duper :integer) {:stack-types [:integer]}))
(define-registered float_dup (with-meta (duper :float) {:stack-types [:float]}))
(define-registered code_dup (with-meta (duper :code) {:stack-types [:code]}))
(define-registered boolean_dup (with-meta (duper :boolean) {:stack-types [:boolean]}))
(define-registered zip_dup (with-meta (duper :zip) {:stack-types [:zip]}))
(define-registered string_dup (with-meta (duper :string) {:stack-types [:string]}))
(define-registered char_dup (with-meta (duper :char) {:stack-types [:char]}))

(defn dup-timeser
  "For integer argument n, duplicate n copies of the top item on the stack, leaving
   n copies there. For n=2, this is equivalent to dup. For n=0, equivalent to pop. Negative
   n values are treated as 0. The maximum items added to the stack will be limited by
   global-max-points."
  [type]
  (fn [state]
    (if (or (and (= type :integer)
                 (>= (count (:integer state)) 2))
            (and (not (= type :integer))
                 (not (empty? (type state)))
                 (not (empty? (:integer state)))))
      (let [times-duplicated (min (top-item :integer state)
                                  (- @global-max-points (count (rest (type (pop-item :integer state))))))
            new-type-stack (not-lazy (concat (repeat times-duplicated
                                                     (top-item type (pop-item :integer state)))
                                             (rest (type (pop-item :integer state)))))]
        (assoc (pop-item :integer state)
               type
               new-type-stack))
      state)))

(define-registered exec_dup_times (with-meta (dup-timeser :exec) {:stack-types [:exec :integer] :parentheses 1}))
(define-registered integer_dup_times (with-meta (dup-timeser :integer) {:stack-types [:integer]}))
(define-registered float_dup_times (with-meta (dup-timeser :float) {:stack-types [:float :integer]}))
(define-registered code_dup_times (with-meta (dup-timeser :code) {:stack-types [:code :integer]}))
(define-registered boolean_dup_times (with-meta (dup-timeser :boolean) {:stack-types [:boolean :integer]}))
(define-registered zip_dup_times (with-meta (dup-timeser :zip) {:stack-types [:zip :integer]}))
(define-registered string_dup_times (with-meta (dup-timeser :string) {:stack-types [:string :integer]}))
(define-registered char_dup_times (with-meta (dup-timeser :char) {:stack-types [:char :integer]}))

(defn dup-itemser
  "For integer argument n, duplicate the top n items on the stack one time each.
   If n <= 0, no items will be duplicated. If fewer than n items are on the stack, the
   entire stack will be duplicated. The maximum number of items to duplicate
   will be limited by global-max-points.
     ex: (3 1.0 2.0 3.0 4.0 5.0 float_dup_items) ->
          (5.0 4.0 3.0 5.0 4.0 3.0 2.0 1.0) on the float stack"
  [type]
  (fn [state]
    (if (empty? (:integer state))
      state
      (let [items-to-duplicate (min (top-item :integer state)
                                    (- @global-max-points (count (type (pop-item :integer state)))))
            new-type-stack (not-lazy (concat (take items-to-duplicate
                                             (type (pop-item :integer state)))
                                     (type (pop-item :integer state))))]
        (assoc (pop-item :integer state)
               type
               new-type-stack)))))

(define-registered exec_dup_items (with-meta (dup-itemser :exec) {:stack-types [:exec :integer] :parentheses 0}))
(define-registered integer_dup_items (with-meta (dup-itemser :integer) {:stack-types [:integer]}))
(define-registered float_dup_items (with-meta (dup-itemser :float) {:stack-types [:float :integer]}))
(define-registered code_dup_items (with-meta (dup-itemser :code) {:stack-types [:code :integer]}))
(define-registered zip_dup_items (with-meta (dup-itemser :zip) {:stack-types [:zip :integer]}))
(define-registered boolean_dup_items (with-meta (dup-itemser :boolean) {:stack-types [:boolean :integer]}))
(define-registered string_dup_items (with-meta (dup-itemser :string) {:stack-types [:string :integer]}))
(define-registered char_dup_items (with-meta (dup-itemser :char) {:stack-types [:char :integer]}))

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

(define-registered exec_swap (with-meta (swapper :exec) {:stack-types [:exec] :parentheses 2}))
(define-registered integer_swap (with-meta (swapper :integer) {:stack-types [:integer]}))
(define-registered float_swap (with-meta (swapper :float) {:stack-types [:float]}))
(define-registered code_swap (with-meta (swapper :code) {:stack-types [:code]}))
(define-registered boolean_swap (with-meta (swapper :boolean) {:stack-types [:boolean]}))
(define-registered zip_swap (with-meta (swapper :zip) {:stack-types [:zip]}))
(define-registered string_swap (with-meta (swapper :string) {:stack-types [:string]}))
(define-registered char_swap (with-meta (swapper :char) {:stack-types [:char]}))

(defn rotter
  "Returns a function that takes a state and rotates the top 3 items of the appropriate 
   stack of the state."
  [type]
  (fn [state]
    (if (not (empty? (rest (rest (type state)))))
      (let [first-item (stack-ref type 0 state)
            second-item (stack-ref type 1 state)
            third-item (stack-ref type 2 state)]
        (->> (pop-item type state)
             (pop-item type)
             (pop-item type)
             (push-item second-item type)
             (push-item first-item type)
             (push-item third-item type)))
      state)))

(define-registered exec_rot (with-meta (rotter :exec) {:stack-types [:exec] :parentheses 3}))
(define-registered integer_rot (with-meta (rotter :integer) {:stack-types [:integer]}))
(define-registered float_rot (with-meta (rotter :float) {:stack-types [:float]}))
(define-registered code_rot (with-meta (rotter :code) {:stack-types [:code]}))
(define-registered boolean_rot (with-meta (rotter :boolean) {:stack-types [:boolean]}))
(define-registered zip_rot (with-meta (rotter :zip) {:stack-types [:zip]}))
(define-registered string_rot (with-meta (rotter :string) {:stack-types [:string]}))
(define-registered char_rot (with-meta (rotter :char) {:stack-types [:char]}))

(defn flusher
  "Returns a function that empties the stack of the given state."
  [type]
  (fn [state]
    (assoc state type '())))

(define-registered exec_flush (with-meta (flusher :exec) {:stack-types [:exec]}))
(define-registered integer_flush (with-meta (flusher :integer) {:stack-types [:integer]}))
(define-registered float_flush (with-meta (flusher :float) {:stack-types [:float]}))
(define-registered code_flush (with-meta (flusher :code) {:stack-types [:code]}))
(define-registered boolean_flush (with-meta (flusher :boolean) {:stack-types [:boolean]}))
(define-registered zip_flush (with-meta (flusher :zip) {:stack-types [:zip]}))
(define-registered string_flush (with-meta (flusher :string) {:stack-types [:string]}))
(define-registered char_flush (with-meta (flusher :char) {:stack-types [:char]}))

(defn eqer
  "Returns a function that compares the top two items of the appropriate stack of 
   the given state."
  [type]
  (fn [state]
    (if (not (empty? (rest (type state))))
      (let [first-item (stack-ref type 0 state)
            second-item (stack-ref type 1 state)]
        (->> (pop-item type state)
             (pop-item type)
             (push-item (= first-item second-item) :boolean)))
      state)))

(define-registered exec_eq (with-meta (eqer :exec) {:stack-types [:exec :boolean] :parentheses 0}))
(define-registered integer_eq (with-meta (eqer :integer) {:stack-types [:integer :boolean]}))
(define-registered float_eq (with-meta (eqer :float) {:stack-types [:float :boolean]}))
(define-registered code_eq (with-meta (eqer :code) {:stack-types [:code :boolean]}))
(define-registered boolean_eq (with-meta (eqer :boolean) {:stack-types [:boolean]}))
(define-registered zip_eq (with-meta (eqer :zip) {:stack-types [:zip :boolean]}))
(define-registered string_eq (with-meta (eqer :string) {:stack-types [:string :boolean]}))
(define-registered char_eq (with-meta (eqer :char) {:stack-types [:char :boolean]}))

(defn stackdepther
  "Returns a function that pushes the depth of the appropriate stack of the 
   given state."
  [type]
  (fn [state]
    (push-item (count (type state)) :integer state)))

(define-registered exec_stackdepth (with-meta (stackdepther :exec) {:stack-types [:exec :integer]}))
(define-registered integer_stackdepth (with-meta (stackdepther :integer) {:stack-types [:integer]}))
(define-registered float_stackdepth (with-meta (stackdepther :float) {:stack-types [:float :integer]}))
(define-registered code_stackdepth (with-meta (stackdepther :code) {:stack-types [:code :integer]}))
(define-registered boolean_stackdepth (with-meta (stackdepther :boolean) {:stack-types [:boolean :integer]}))
(define-registered zip_stackdepth (with-meta (stackdepther :zip) {:stack-types [:zip :integer]}))
(define-registered string_stackdepth (with-meta (stackdepther :string) {:stack-types [:string :integer]}))
(define-registered char_stackdepth (with-meta (stackdepther :char) {:stack-types [:char :integer]}))

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

(define-registered exec_yank (with-meta (yanker :exec) {:stack-types [:exec :integer] :parentheses 0}))
(define-registered integer_yank (with-meta (yanker :integer) {:stack-types [:integer]}))
(define-registered float_yank (with-meta (yanker :float) {:stack-types [:float :integer]}))
(define-registered code_yank (with-meta (yanker :code) {:stack-types [:code :integer]}))
(define-registered boolean_yank (with-meta (yanker :boolean) {:stack-types [:boolean :integer]}))
(define-registered zip_yank (with-meta (yanker :zip) {:stack-types [:zip :integer]}))
(define-registered string_yank (with-meta (yanker :string) {:stack-types [:string :integer]}))
(define-registered char_yank (with-meta (yanker :char) {:stack-types [:char :integer]}))

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

(define-registered exec_yankdup (with-meta (yankduper :exec) {:stack-types [:exec :integer] :parentheses 0}))
(define-registered integer_yankdup (with-meta (yankduper :integer) {:stack-types [:integer]}))
(define-registered float_yankdup (with-meta (yankduper :float) {:stack-types [:float :integer]}))
(define-registered code_yankdup (with-meta (yankduper :code) {:stack-types [:code :integer]}))
(define-registered boolean_yankdup (with-meta (yankduper :boolean) {:stack-types [:boolean :integer]}))
(define-registered zip_yankdup (with-meta (yankduper :zip) {:stack-types [:zip :integer]}))
(define-registered string_yankdup (with-meta (yankduper :string) {:stack-types [:string :integer]}))
(define-registered char_yankdup (with-meta (yankduper :char) {:stack-types [:char :integer]}))

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

(define-registered exec_shove (with-meta (shover :exec) {:stack-types [:exec :integer] :parentheses 1}))
(define-registered integer_shove (with-meta (shover :integer) {:stack-types [:integer]}))
(define-registered float_shove (with-meta (shover :float) {:stack-types [:float :integer]}))
(define-registered code_shove (with-meta (shover :code) {:stack-types [:code :integer]}))
(define-registered boolean_shove (with-meta (shover :boolean) {:stack-types [:boolean :integer]}))
(define-registered zip_shove (with-meta (shover :zip) {:stack-types [:zip :integer]}))
(define-registered string_shove (with-meta (shover :string) {:stack-types [:string :integer]}))
(define-registered char_shove (with-meta (shover :char) {:stack-types [:char :integer]}))

(defn emptyer
  "Returns a function that takes a state and tells whether that stack is empty."
  [type]
  (fn [state]
    (push-item (empty? (type state))
               :boolean
               state)))

(define-registered exec_empty (with-meta (emptyer :exec) {:stack-types [:exec :boolean]}))
(define-registered integer_empty (with-meta (emptyer :integer) {:stack-types [:integer :boolean]}))
(define-registered float_empty (with-meta (emptyer :float) {:stack-types [:float :boolean]}))
(define-registered code_empty (with-meta (emptyer :code) {:stack-types [:code :boolean]}))
(define-registered boolean_empty (with-meta (emptyer :boolean) {:stack-types [:boolean]}))
(define-registered zip_empty (with-meta (emptyer :zip) {:stack-types [:zip :boolean]}))
(define-registered string_empty (with-meta (emptyer :string) {:stack-types [:string :boolean]}))
(define-registered char_empty (with-meta (emptyer :char) {:stack-types [:char :boolean]}))
