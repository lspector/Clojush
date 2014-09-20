(ns clojush.pushstate
  (:use [clojush.globals]
        [clojure.set]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; states, stacks, and instructions

;; struct-based states follow

;(defmacro define-push-state-structure []
;  `(defstruct push-state ~@push-types))

;(define-push-state-structure)

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
    (println (t state))
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

(def instruction-types
  "A map of the stack types required for each instruction. For example,
   integer_fromboolean requires the boolean and integer stacks. If an instruction
   is non-deterministic, it will include :random in its list."
  '{boolean_and [:boolean]
    boolean_dup [:boolean]
    boolean_empty [:boolean]
    boolean_eq [:boolean]
    boolean_flush [:boolean]
    boolean_fromfloat [:boolean :float]
    boolean_frominteger [:boolean :integer]
    boolean_invert_first_then_and [:boolean]
    boolean_invert_second_then_and [:boolean]
    boolean_not [:boolean]
    boolean_or [:boolean]
    boolean_pop [:boolean]
    boolean_rand [:boolean :random]
    boolean_rot [:boolean]
    boolean_shove [:boolean :integer]
    boolean_stackdepth [:boolean :integer]
    boolean_swap [:boolean]
    boolean_xor [:boolean]
    boolean_yank [:boolean :integer]
    boolean_yankdup [:boolean :integer]
    char_allfromstring [:char :string]
    char_dup [:char]
    char_empty [:boolean :char]
    char_eq [:char :boolean]
    char_flush [:char]
    char_frominteger [:char :integer]
    char_fromfloat [:char :float]
    char_isletter [:char :boolean]
    char_isdigit [:char :boolean]
    char_iswhitespace [:char :boolean]
    char_pop [:char]
    char_rand [:char :random]
    char_rot [:char]
    char_shove [:char :integer]
    char_stackdepth [:char :integer]
    char_swap [:char]
    char_yank [:char :integer]
    char_yankdup [:char :integer]
    code_append [:code]
    code_atom [:code :boolean]
    code_car [:code]
    code_cdr [:code]
    code_cons [:code]
    code_container [:code]
    code_contains [:code :boolean]
    code_do [:code :exec]
    code_do* [:code :exec]
    code_do*count [:code :exec :integer]
    code_do*range [:code :exec :integer]
    code_do*times [:code :exec :integer]
    code_dup [:code]
    code_empty [:boolean :code]
    code_eq [:code :boolean]
    code_extract [:code :integer]
    code_flush [:code]
    code_fromboolean [:code :boolean]
    code_fromfloat [:code :float]
    code_frominteger [:code :integer]
    code_fromzipchildren [:code :zip]
    code_fromziplefts [:code :zip]
    code_fromzipnode [:code :zip]
    code_fromziprights [:code :zip]
    code_fromziproot [:code :zip]
    code_if [:code :boolean :exec]
    code_insert [:code :integer]
    code_length [:code :integer]
    code_list [:code]
    code_map [:code :exec]
    code_member [:code :boolean]
    code_noop [:code]
    code_nth [:code :integer]
    code_nthcdr [:code :integer]
    code_null [:code :boolean]
    code_pop [:code]
    code_position [:code :integer]
    code_quote [:code :exec]
    code_rand [:code :integer :random]
    code_rot [:code]
    code_shove [:code :integer]
    code_size [:code :integer]
    code_stackdepth [:code :integer]
    code_subst [:code]
    code_swap [:code]
    code_wrap [:code]
    code_yank [:code :integer]
    code_yankdup [:code :integer]
    environment_begin [:environment]
    environment_end [:environment]
    environment_new [:environment]
    exec_do*count [:exec :integer]
    exec_do*range [:exec :integer]
    exec_do*times [:exec :integer]
    exec_do*while [:exec :boolean]
    exec_dup [:exec]
    exec_empty [:boolean :exec]
    exec_eq [:exec :boolean]
    exec_flush [:exec]
    exec_fromzipchildren [:exec :zip]
    exec_fromziplefts [:exec :zip]
    exec_fromzipnode [:exec :zip]
    exec_fromziprights [:exec :zip]
    exec_fromziproot [:exec :zip]
    exec_if [:exec :boolean]
    exec_k [:exec]
    exec_noop [:exec]
    exec_pop [:exec]
    exec_rot [:exec]
    exec_s [:exec]
    exec_shove [:exec :integer]
    exec_stackdepth [:exec :integer]
    exec_swap [:exec]
    exec_when [:exec :boolean]
    exec_while [:exec :boolean]
    exec_y [:exec]
    exec_yank [:exec :integer]
    exec_yankdup [:exec :integer]
    float_add [:float]
    float_cos [:float]
    float_dec [:float]
    float_div [:float]
    float_dup [:float]
    float_empty [:boolean :float]
    float_eq [:float :boolean]
    float_flush [:float]
    float_fromboolean [:float :boolean]
    float_fromchar [:float :char]
    float_frominteger [:float :integer]
    float_fromstring [:string :float]
    float_gt [:float :boolean]
    float_gte [:float :boolean]
    float_inc [:float]
    float_lt [:float :boolean]
    float_lte [:float :boolean]
    float_max [:float]
    float_min [:float]
    float_mod [:float]
    float_mult [:float]
    float_pop [:float]
    float_rand [:float :random]
    float_rot [:float]
    float_shove [:float :integer]
    float_sin [:float]
    float_stackdepth [:float :integer]
    float_sub [:float]
    float_swap [:float]
    float_tan [:float]
    float_yank [:float :integer]
    float_yankdup [:float :integer]
    integer_add [:integer]
    integer_dec [:integer]
    integer_div [:integer]
    integer_dup [:integer]
    integer_empty [:boolean :integer]
    integer_eq [:integer :boolean]
    integer_flush [:integer]
    integer_fromboolean [:integer :boolean]
    integer_fromchar [:integer :char]
    integer_fromfloat [:integer :float]
    integer_fromstring [:string :integer]
    integer_gt [:integer :boolean]
    integer_gte [:integer :boolean]
    integer_inc [:integer]
    integer_lt [:integer :boolean]
    integer_lte [:integer :boolean]
    integer_max [:integer]
    integer_min [:integer]
    integer_mod [:integer]
    integer_mult [:integer]
    integer_pop [:integer]
    integer_rand [:integer :random]
    integer_rot [:integer]
    integer_shove [:integer]
    integer_stackdepth [:integer]
    integer_sub [:integer]
    integer_swap [:integer]
    integer_yank [:integer]
    integer_yankdup [:integer]
    noop_delete_prev_paren_pair [:parentheses]
    noop_open_paren [:parentheses]
    print_boolean [:print :boolean]
    print_char [:print :char]
    print_code [:print :code]
    print_exec [:print :exec]
    print_float [:print :float]
    print_integer [:print :integer]
    print_newline [:print]
    print_string [:print :string]
    print_vector_integer [:print :vector_integer]
    print_vector_float [:print :vector_float]
    print_vector_boolean [:print :vector_boolean]
    print_vector_string [:print :vector_string]
    return_boolean_pop [:environment :boolean]
    return_code_pop [:environment :code]
    return_exec_pop [:environment :exec]
    return_float_pop [:environment :float]
    return_fromboolean [:environment :boolean]
    return_fromcode [:environment :code]
    return_fromexec [:environment :exec]
    return_fromfloat [:environment :float]
    return_frominteger [:environment :integer]
    return_fromstring [:environment :string]
    return_integer_pop [:environment :integer]
    return_string_pop [:environment :string]
    return_tagspace [:environment]
    return_zip_pop [:environment :zip]
    string_butlast [:string]
    string_concat [:string]
    string_conjchar [:string :char]
    string_contains [:string :boolean]
    string_containschar [:string :boolean :char]
    string_dup [:string]
    string_empty [:boolean :string]
    string_emptystring [:boolean :string]
    string_eq [:string :boolean]
    string_first [:string :char]
    string_flush [:string]
    string_fromboolean [:string :boolean]
    string_fromchar [:string :char]
    string_fromfloat [:string :float]
    string_frominteger [:string :integer]
    string_indexofchar [:string :integer :char]
    string_last [:string :char]
    string_length [:string :integer]
    string_nth [:string :integer :char]
    string_occurrencesofchar [:string :integer :char]
    string_parse_to_chars [:string]
    string_pop [:string]
    string_rand [:string :random]
    string_removechar [:string :char]
    string_replace [:string]
    string_replacefirst [:string]
    string_replacefirstchar [:string :char]
    string_replacechar [:string :char]
    string_rest [:string]
    string_reverse [:string]
    string_rot [:string]
    string_shove [:string :integer]
    string_split [:string]
    string_stackdepth [:string :integer]
    string_substring [:string :integer]
    string_swap [:string]
    string_take [:string :integer]
    string_yank [:string :integer]
    string_yankdup [:string :integer]
    vector_boolean_butlast [:vector_boolean]
    vector_boolean_concat [:vector_boolean]
    vector_boolean_conj [:vector_boolean :boolean]
    vector_boolean_contains [:vector_boolean :boolean]
    vector_boolean_dup [:vector_boolean]
    vector_boolean_empty [:vector_boolean :boolean]
    vector_boolean_emptyvector [:vector_boolean :boolean]
    vector_boolean_eq [:vector_boolean :boolean]
    vector_boolean_first [:vector_boolean :boolean]
    vector_boolean_flush [:vector_boolean]
    vector_boolean_indexof [:vector_boolean :boolean :integer]
    vector_boolean_last [:vector_boolean :boolean]
    vector_boolean_length [:vector_boolean :integer]
    vector_boolean_nth [:vector_boolean :boolean :integer]
    vector_boolean_occurrencesof [:vector_boolean :boolean :integer]
    vector_boolean_pop [:vector_boolean]
    vector_boolean_pushall [:vector_boolean :boolean]
    vector_boolean_remove [:vector_boolean :boolean]
    vector_boolean_replace [:vector_boolean :boolean]
    vector_boolean_replacefirst [:vector_boolean :boolean]
    vector_boolean_rest [:vector_boolean]
    vector_boolean_reverse [:vector_boolean]
    vector_boolean_rot [:vector_boolean]
    vector_boolean_shove [:vector_boolean :integer]
    vector_boolean_stackdepth [:vector_boolean :integer]
    vector_boolean_subvec [:vector_boolean :integer]
    vector_boolean_swap [:vector_boolean]
    vector_boolean_take [:vector_boolean :integer]
    vector_boolean_yank [:vector_boolean :integer]
    vector_boolean_yankdup [:vector_boolean :integer]
    vector_float_butlast [:vector_float]
    vector_float_concat [:vector_float]
    vector_float_conj [:vector_float :float]
    vector_float_contains [:vector_float :boolean :float]
    vector_float_dup [:vector_float]
    vector_float_empty [:vector_float :boolean]
    vector_float_emptyvector [:vector_float :boolean]
    vector_float_eq [:vector_float :boolean]
    vector_float_first [:vector_float :float]
    vector_float_flush [:vector_float]
    vector_float_indexof [:vector_float :float :integer]
    vector_float_last [:vector_float :float]
    vector_float_length [:vector_float :integer]
    vector_float_nth [:vector_float :float :integer]
    vector_float_occurrencesof [:vector_float :float :integer]
    vector_float_pop [:vector_float]
    vector_float_pushall [:vector_float :float]
    vector_float_remove [:vector_float :float]
    vector_float_replace [:vector_float :float]
    vector_float_replacefirst [:vector_float :float]
    vector_float_rest [:vector_float]
    vector_float_reverse [:vector_float]
    vector_float_rot [:vector_float]
    vector_float_shove [:vector_float :integer]
    vector_float_stackdepth [:vector_float :integer]
    vector_float_subvec [:vector_float :integer]
    vector_float_swap [:vector_float]
    vector_float_take [:vector_float :integer]
    vector_float_yank [:vector_float :integer]
    vector_float_yankdup [:vector_float :integer]
    vector_integer_butlast [:vector_integer]
    vector_integer_concat [:vector_integer]
    vector_integer_conj [:vector_integer :integer]
    vector_integer_contains [:vector_integer :integer :boolean]
    vector_integer_dup [:vector_integer]
    vector_integer_empty [:vector_integer :boolean]
    vector_integer_emptyvector [:vector_integer :boolean]
    vector_integer_eq [:vector_integer :boolean]
    vector_integer_first [:vector_integer :integer]
    vector_integer_flush [:vector_integer]
    vector_integer_indexof [:vector_integer :integer]
    vector_integer_last [:vector_integer :integer]
    vector_integer_length [:vector_integer :integer]
    vector_integer_nth [:vector_integer :integer]
    vector_integer_occurrencesof [:vector_integer :integer]
    vector_integer_pop [:vector_integer]
    vector_integer_pushall [:vector_integer :integer]
    vector_integer_remove [:vector_integer :integer]
    vector_integer_replace [:vector_integer :integer]
    vector_integer_replacefirst [:vector_integer :integer]
    vector_integer_rest [:vector_integer]
    vector_integer_reverse [:vector_integer]
    vector_integer_rot [:vector_integer]
    vector_integer_shove [:vector_integer :integer]
    vector_integer_stackdepth [:vector_integer :integer]
    vector_integer_subvec [:vector_integer :integer]
    vector_integer_swap [:vector_integer]
    vector_integer_take [:vector_integer :integer]
    vector_integer_yank [:vector_integer :integer]
    vector_integer_yankdup [:vector_integer :integer]
    vector_string_butlast [:vector_string]
    vector_string_concat [:vector_string]
    vector_string_conj [:vector_string :string]
    vector_string_contains [:vector_string :string :boolean]
    vector_string_dup [:vector_string]
    vector_string_empty [:vector_string :boolean]
    vector_string_emptyvector [:vector_string :boolean]
    vector_string_eq [:vector_string :boolean]
    vector_string_first [:vector_string :string]
    vector_string_flush [:vector_string]
    vector_string_indexof [:vector_string :string :integer]
    vector_string_last [:vector_string :string]
    vector_string_length [:vector_string :integer]
    vector_string_nth [:vector_string :string :integer]
    vector_string_occurrencesof [:vector_string :string :integer]
    vector_string_pop [:vector_string]
    vector_string_pushall [:vector_string :string]
    vector_string_remove [:vector_string :string]
    vector_string_replace [:vector_string :string]
    vector_string_replacefirst [:vector_string :string]
    vector_string_rest [:vector_string]
    vector_string_reverse [:vector_string]
    vector_string_rot [:vector_string]
    vector_string_shove [:vector_string :integer]
    vector_string_stackdepth [:vector_string :integer]
    vector_string_subvec [:vector_string :integer]
    vector_string_swap [:vector_string]
    vector_string_take [:vector_string :integer]
    vector_string_yank [:vector_string :integer]
    vector_string_yankdup [:vector_string :integer]
    zip_append_child_fromcode [:zip :code]
    zip_append_child_fromexec [:zip :exec]
    zip_branch? [:zip :boolean]
    zip_down [:zip]
    zip_dup [:zip]
    zip_empty [:boolean :zip]
    zip_end? [:zip :boolean]
    zip_eq [:zip :boolean]
    zip_flush [:zip]
    zip_fromcode [:zip :code]
    zip_fromexec [:zip :exec]
    zip_insert_child_fromcode [:zip :code]
    zip_insert_child_fromexec [:zip :exec]
    zip_insert_left_fromcode [:zip :code]
    zip_insert_left_fromexec [:zip :exec]
    zip_insert_right_fromcode [:zip :code]
    zip_insert_right_fromexec [:zip :exec]
    zip_left [:zip]
    zip_leftmost [:zip]
    zip_next [:zip]
    zip_pop [:zip]
    zip_prev [:zip]
    zip_remove [:zip]
    zip_replace_fromcode [:zip :code]
    zip_replace_fromexec [:zip :exec]
    zip_right [:zip]
    zip_rightmost [:zip]
    zip_rot [:zip]
    zip_shove [:zip :integer]
    zip_stackdepth [:zip :integer]
    zip_swap [:zip]
    zip_up [:zip]
    zip_yank [:zip :integer]
    zip_yankdup [:zip :integer]
    })
  
(defn registered-for-stacks
  "Takes a list of stacks and returns all instructions that have all
   of their stack requirements fulfilled. This won't include random instructions
   unless :random is in the types list. This won't include parenthesis-altering
   instructions unless :parentheses is in the types list."
  [types-list]
  (map first
       (filter (fn [[instr req-types]]
                 (clojure.set/subset? (set req-types) (set types-list)))
               instruction-types)))

(defn registered-for-stacks-meta
  "Takes a list of stacks and returns all instructions that have all
   of their stack requirements fulfilled. This won't include random instructions
   unless :random is in the types list. This won't include parenthesis-altering
   instructions unless :parentheses is in the types list."
  [types-list]
  (map first
       (filter (fn [[instr instr-fn]]
                 (and (:stack-types (meta instr-fn))
                      (clojure.set/subset? (set (:stack-types (meta instr-fn))) (set types-list))))
               @instruction-table)))

; The following finds instructions that haven't yet been added to instruction-types
(defn print-instructions-without-types
  []
  (let [ins-registered-for-stacks (set (registered-for-stacks-meta [:integer :boolean :float :char :string :print :random
                                                                    :parentheses :exec :code :vector_integer :vector_string
                                                                    :vector_boolean :vector_float :zip :environment]))]
    (println "===============================")
    (println "The following are in @registered-instructions but not in registed-for-stacks-meta (and should be added)")
    (println "===============================")
    (doseq [ins (sort-by name (clojure.set/difference (set @registered-instructions)
                                                      ins-registered-for-stacks))]
      (println ins))
    (println "===============================")
    (println "The following are in registered-for-stacks-meta but not in @registered-instructions")
    (println "===============================")
    (doseq [ins (sort-by name (clojure.set/difference ins-registered-for-stacks
                                                      (set @registered-instructions)))]
      (println ins))))

(defn print-instructions-with-diff-meta
  []
  (let [ins-registered-for-stacks (sort-by name (registered-for-stacks-meta [:integer :boolean :float :char :string :print :random
                                                                             :parentheses :exec :code :vector_integer :vector_string
                                                                             :vector_boolean :vector_float :zip :environment]))]
    (doseq [x (filter #(not (= (sort (second %)) (sort (nth % 2))))
                      (map (fn [ins]
                            (vector ins
                                    (:stack-types (meta (get @instruction-table ins)))
                                    (get instruction-types ins)
                                    ))
                          ins-registered-for-stacks))]
      (println x))))
