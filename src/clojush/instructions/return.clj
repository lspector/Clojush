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
(define-registered return_fromzip (returner :zip))
(define-registered return_fromstring (returner :string))

(define-registered 
  return_fromcode
  (fn [state]
    (if (empty? (:code state))
      state
      (let [item (list 'code_quote (top-item :code state))]
        (push-item item :return (pop-item :code state))))))
