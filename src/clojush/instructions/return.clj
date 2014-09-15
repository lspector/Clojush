(ns clojush.instructions.return
  (:use [clojush.pushstate]))


(defn returner
  "Returns a function that takes a state and moves the top literal
   from the appropriate stack to the return stack."
  [type]
  (fn [state]
    (if (empty? (type state))
      state
      (let [item (top-item type state)]
        (push-item item :return (pop-item type state))))))

(define-registered return_fromexec (returner :exec))
(define-registered return_frominteger (returner :integer))
(define-registered return_fromfloat (returner :float))
(define-registered return_fromboolean (returner :boolean))
;(define-registered return_fromzip (returner :zip)) ;; won't work, reconsider
(define-registered return_fromstring (returner :string))

(define-registered
  return_fromcode
  (fn [state]
    (if (empty? (:code state))
      state
      (let [item (list 'code_quote (top-item :code state))]
        (push-item item :return (pop-item :code state))))))

(define-registered
  return_exec_pop
  (fn [state]
    (assoc state :return (concat (:return state)
                                 (list 'exec_pop)))))

(define-registered
  return_code_pop
  (fn [state]
    (assoc state :return (concat (:return state)
                                 (list 'code_pop)))))

(define-registered
  return_integer_pop
  (fn [state]
    (assoc state :return (concat (:return state)
                                 (list 'integer_pop)))))

(define-registered
  return_float_pop
  (fn [state]
    (assoc state :return (concat (:return state)
                                 (list 'float_pop)))))

(define-registered
  return_boolean_pop
  (fn [state]
    (assoc state :return (concat (:return state)
                                 (list 'boolean_pop)))))

(define-registered
  return_zip_pop
  (fn [state]
    (assoc state :return (concat (:return state)
                                 (list 'zip_pop)))))

(define-registered
  return_string_pop
  (fn [state]
    (assoc state :return (concat (:return state)
                                 (list 'string_pop)))))

; Immediately copies the current tagspace to the environment on the top
; of the :environment stack.
(define-registered
  return_tagspace
  (fn [state]
    (if (empty? (:environment state))
      state
      (let [top-env (top-item :environment state)
            new-env (assoc top-env :tag (:tag state))]
        (push-item new-env :environment (pop-item :environment state))))))
