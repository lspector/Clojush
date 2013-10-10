(ns clojush.instructions.zip
  (:use [clojush.pushstate]
        [clojush.util]
        [clojush.globals])
  (:require [clojure.zip :as zip]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; zip instructions

(defmacro ignore-errors
  "Returns the result of evaluating e, or nil if it throws an exception."
  [e]
  `(try ~e (catch java.lang.Exception _# nil)))

(defn zip-mover
  "Returns a function that moves the top zipper in the specified way,
   acting as a no-op if the movement would produce an error."
  [move-fn]
  (fn [state]
    (if (empty? (:zip state))
      state
      (let [result (ignore-errors (move-fn (top-item :zip state)))]
        (if (or (nil? result) (not (vector? result)))
          state
          (push-item result :zip (pop-item :zip state)))))))

(define-registered zip_next (zip-mover zip/next))
(define-registered zip_prev (zip-mover zip/prev))
(define-registered zip_down (zip-mover zip/down))
(define-registered zip_up (zip-mover zip/up))
(define-registered zip_left (zip-mover zip/left))
(define-registered zip_leftmost (zip-mover zip/leftmost))
(define-registered zip_right (zip-mover zip/right))
(define-registered zip_rightmost (zip-mover zip/rightmost))

(defn zip-tester
  [test-fn]
  (fn [state]
    (if (empty? (:zip state))
      state
      (let [result (ignore-errors (test-fn (top-item :zip state)))]
        (if (nil? result)
          state
          (push-item result :boolean (pop-item :zip state)))))))

(define-registered zip_end? (zip-tester zip/end?))
(define-registered zip_branch? (zip-tester zip/branch?))

(defn zip-inserter
  [source inserter]
  (fn [state]
    (if (or (empty? (:zip state)) (empty? (source state)))
      state
      (let [z (stack-ref :zip 0 state)
            c (stack-ref source 0 state)
            result (ignore-errors (inserter z c))]
        (if (and result (<= (count-points (zip/root result)) @global-max-points))
          (push-item result :zip (pop-item :zip (pop-item source state)))
          state)))))

(define-registered zip_replace_fromcode (zip-inserter :code zip/replace))
(define-registered zip_replace_fromexec (zip-inserter :exec zip/replace))

(define-registered zip_insert_right_fromcode (zip-inserter :code zip/insert-right))
(define-registered zip_insert_right_fromexec (zip-inserter :exec zip/insert-right))

(define-registered zip_insert_left_fromcode (zip-inserter :code zip/insert-left))
(define-registered zip_insert_left_fromexec (zip-inserter :exec zip/insert-left))

(define-registered zip_insert_child_fromcode (zip-inserter :code zip/insert-child))
(define-registered zip_insert_child_fromexec (zip-inserter :exec zip/insert-child))

(define-registered zip_append_child_fromcode (zip-inserter :code zip/append-child))
(define-registered zip_append_child_fromexec (zip-inserter :exec zip/append-child))

(define-registered 
  zip_remove
  (fn [state]
    (if (empty? (:zip state))
      state
      (let [result (ignore-errors (zip/remove (top-item :zip state)))]
        (if result
          (push-item result :zip (pop-item :zip state))
          state)))))

(define-registered 
  zip_fromcode
  (fn [state]
    (if (empty? (:code state))
      state
      (let [result (ignore-errors (zip/seq-zip (top-item :code state)))]
        (if result
          (push-item result :zip (pop-item :code state))
          state)))))

(define-registered 
  zip_fromexec
  (fn [state]
    (if (empty? (:exec state))
      state
      (let [result (ignore-errors (zip/seq-zip (top-item :exec state)))]
        (if result
          (push-item result :zip (pop-item :exec state))
          state)))))

(defn zip-extractor
  [destination extractor]
  (fn [state]
    (if (empty? (:zip state))
      state
      (let [z (stack-ref :zip 0 state)
            result (ignore-errors (extractor z))]
        (if result
          (push-item result destination (pop-item :zip state))
          state)))))

(define-registered code_fromzipnode (zip-extractor :code zip/node))
(define-registered exec_fromzipnode (zip-extractor :exec zip/node))

(define-registered code_fromziproot (zip-extractor :code zip/root))
(define-registered exec_fromziproot (zip-extractor :exec zip/root))

(define-registered code_fromzipchildren (zip-extractor :code zip/children))
(define-registered exec_fromzipchildren (zip-extractor :exec zip/children))

(define-registered code_fromziplefts (zip-extractor :code zip/lefts))
(define-registered exec_fromziplefts (zip-extractor :exec zip/lefts))

(define-registered code_fromziprights (zip-extractor :code zip/rights))
(define-registered exec_fromziprights (zip-extractor :exec zip/rights))
