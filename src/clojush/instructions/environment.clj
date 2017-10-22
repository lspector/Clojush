(ns clojush.instructions.environment
  (:use [clojush pushstate util]))

(define-registered
  environment_new
  ^{:stack-types [:environment]
    :parentheses 1}
  ;; Creates new environment using the top item on the exec stack
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
  ^{:stack-types [:environment]}
  ;; Creates new environment using the entire exec stack
  (fn [state]
    (assoc (push-item (assoc state :exec '())
                      :environment state)
           :return '())))

(define-registered
  environment_end
  ^{:stack-types [:environment]}
  ;; Ends current environment
  (fn [state]
    (if (empty? (:environment state))
      state
      (end-environment state))))

(defn returner
  "Returns a function that takes a state and moves the top literal
   from the appropriate stack to the return stack."
  [type]
  (fn [state]
    (if (empty? (type state))
      state
      (let [item (top-item type state)]
        (push-item {:type type :item item} :return (pop-item type state))))))

(define-registered return_fromexec (with-meta (returner :exec) {:stack-types [:environment :exec] :parentheses 1}))
(define-registered return_fromcode (with-meta (returner :code) {:stack-types [:environment :code]}))
(define-registered return_frominteger (with-meta (returner :integer) {:stack-types [:environment :integer]}))
(define-registered return_fromfloat (with-meta (returner :float) {:stack-types [:environment :float]}))
(define-registered return_fromboolean (with-meta (returner :boolean) {:stack-types [:environment :boolean]}))
(define-registered return_fromzip (with-meta (returner :zip) {:stack-types [:environment :zip]}))
(define-registered return_fromstring (with-meta (returner :string) {:stack-types [:environment :string]}))
(define-registered return_fromchar (with-meta (returner :char) {:stack-types [:environment :char]}))
(define-registered return_fromgenome (with-meta (returner :genome) {:stack-types [:environment :genome]}))

(define-registered
  return_exec_pop
  ^{:stack-types [:environment :exec]}
  (fn [state]
    (assoc state :return (list-concat (list {:type :exec :popper true}) (:return state)))))

(define-registered
  return_code_pop
  ^{:stack-types [:environment :code]}
  (fn [state]
    (assoc state :return (list-concat (list {:type :code :popper true}) (:return state)))))

(define-registered
  return_integer_pop
  ^{:stack-types [:environment :integer]}
  (fn [state]
    (assoc state :return (list-concat (list {:type :integer :popper true}) (:return state)))))

(define-registered
  return_float_pop
  ^{:stack-types [:environment :float]}
  (fn [state]
    (assoc state :return (list-concat (list {:type :float :popper true}) (:return state)))))

(define-registered
  return_boolean_pop
  ^{:stack-types [:environment :boolean]}
  (fn [state]
    (assoc state :return (list-concat (list {:type :boolean :popper true}) (:return state)))))

(define-registered
  return_zip_pop
  ^{:stack-types [:environment :zip]}
  (fn [state]
    (assoc state :return (list-concat (list {:type :zip :popper true}) (:return state)))))

(define-registered
  return_string_pop
  ^{:stack-types [:environment :string]}
  (fn [state]
    (assoc state :return (list-concat (list {:type :string :popper true}) (:return state)))))

(define-registered
  return_char_pop
  ^{:stack-types [:environment :char]}
  (fn [state]
    (assoc state :return (list-concat (list {:type :char :popper true}) (:return state)))))

(define-registered
  return_genome_pop
  ^{:stack-types [:environment :genome]}
  (fn [state]
    (assoc state :return (list-concat (list {:type :genome :popper true}) (:return state)))))

; Immediately copies the current tagspace to the environment on the top
; of the :environment stack.
(define-registered
  return_tagspace
  ^{:stack-types [:environment]}
  (fn [state]
    (if (empty? (:environment state))
      state
      (let [top-env (top-item :environment state)
            new-env (assoc top-env :tag (:tag state))]
        (push-item new-env :environment (pop-item :environment state))))))

