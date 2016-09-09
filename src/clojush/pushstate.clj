(ns clojush.pushstate
  (:use [clojush.globals]
        [clojure.set]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; states, stacks, and instructions

;; struct-based states follow

;(defmacro define-push-state-structure []
;  `(defstruct push-state ~@push-types))
;
;(define-push-state-structure)
;
;(defn make-push-state
;  "Returns an empty push state."
;  []
;  (struct-map push-state))

;; record-based states (apparently faster)

(defn keyword->symbol [kwd]
 "Returns the symbol obtained by removing the : from a keyword."
 (symbol (name kwd)))

(defmacro define-push-state-record-type []
 `(defrecord ~'PushState [~@(map keyword->symbol push-types)]))

(define-push-state-record-type)

(let [empty-state (map->PushState {})]
  (defn make-push-state
    "Returns an empty push state."
    [] empty-state))

(def registered-instructions (atom #{}))

(defn register-instruction
  "Add the provided name to the global list of registered instructions."
  [name]
  (if (some #{name} @registered-instructions)
    (throw (Exception. (str "Duplicate Push instruction defined:" name)))
    (swap! registered-instructions conj name)))

(def instruction-table (atom (hash-map)))

(defmacro define-registered
  [instruction definition]
  `(do (register-instruction '~instruction)
       (swap! instruction-table assoc '~instruction ~definition)))

(defn state-pretty-print
  [state]
  (doseq [t push-types]
    (printf "%s = " t)
    (prn (t state))
    (flush)))

(defn push-item
  "Returns a copy of the state with the value pushed on the named stack. This is a utility,
   not for use in Push programs."
  [value type state]
  (assoc state type (cons value (type state))))

(defn top-item
  "Returns the top item of the type stack in state. Returns :no-stack-item if called on
   an empty stack. This is a utility, not for use as an instruction in Push programs."
  [type state]
  (let [stack (type state)]
    (if (empty? stack)
      :no-stack-item
      (first stack))))

(defn stack-ref
  "Returns the indicated item of the type stack in state. Returns :no-stack-item if called
   on an empty stack. This is a utility, not for use as an instruction in Push programs.
   NOT SAFE for invalid positions."
  [type position state]
  (let [stack (type state)]
    (if (empty? stack)
      :no-stack-item
      (nth stack position))))

(defn stack-assoc
  "Puts value at position on type stack in state. This is a utility, not for use
   as an instruction in Push programs. NOT SAFE for invalid positions."
  [value type position state]
  (let [stack (type state)
        new-stack (apply list (assoc (vec stack) position value))]
    (assoc state type new-stack)))

(defn pop-item
  "Returns a copy of the state with the specified stack popped. This is a utility,
   not for use as an instruction in Push programs."
  [type state]
  (assoc state type (rest (type state))))

(defn end-environment
  "Ends the current environment by popping the :environment stack and replacing
   all stacks with those on the environment stack. Then, everything on the old
   :return stack is pushed onto the :exec stack."
  [state]
  (let [new-env (top-item :environment state)
        new-exec (concat (:exec state)
                         (:exec new-env))]
    (loop [old-return (:return state)
           new-state (assoc new-env
                            :exec new-exec
                            :auxiliary (:auxiliary state))]
      (if (empty? old-return)
        new-state
        (recur (rest old-return)
               (push-item (first old-return) :exec new-state))))))

(defn registered-for-type
  "Returns a list of all registered instructions with the given type name as a prefix."
  [type & {:keys [include-randoms] :or {include-randoms true}}]
  (let [for-type (filter #(.startsWith (name %) (name type)) @registered-instructions)]
    (if include-randoms
      for-type
      (filter #(not (.endsWith (name %) "_rand")) for-type))))

(defn registered-nonrandom
  "Returns a list of all registered instructions aside from random instructions."
  []
  (filter #(not (.endsWith (name %) "_rand")) @registered-instructions))

(defn registered-for-stacks
  "Takes a list of stacks and returns all instructions that have all
   of their stack requirements fulfilled. This won't include random instructions
   unless :random is in the types list. This won't include parenthesis-altering
   instructions unless :parentheses is in the types list."
  [types-list]
  (doseq [[instr instr-fn] @instruction-table]
    (assert (:stack-types (meta instr-fn)) (format "Instruction %s does not have :stack-types defined in metadata." (name instr))))
  (map first
       (filter (fn [[instr instr-fn]]
                 (and (:stack-types (meta instr-fn))
                      (clojure.set/subset? (set (:stack-types (meta instr-fn))) (set types-list))))
               @instruction-table)))


(defn push-state-from-stacks
  "Takes a map of stack names and entire stack states, and returns a new push-state
   with those stacks set."
  [& {:as stack-assignments}]
  (merge (make-push-state) stack-assignments))
    
