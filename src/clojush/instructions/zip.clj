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

(define-registered zip_next (with-meta (zip-mover zip/next) {:stack-types [:zip]}))
(define-registered zip_prev (with-meta (zip-mover zip/prev) {:stack-types [:zip]}))
(define-registered zip_down (with-meta (zip-mover zip/down) {:stack-types [:zip]}))
(define-registered zip_up (with-meta (zip-mover zip/up) {:stack-types [:zip]}))
(define-registered zip_left (with-meta (zip-mover zip/left) {:stack-types [:zip]}))
(define-registered zip_leftmost (with-meta (zip-mover zip/leftmost) {:stack-types [:zip]}))
(define-registered zip_right (with-meta (zip-mover zip/right) {:stack-types [:zip]}))
(define-registered zip_rightmost (with-meta (zip-mover zip/rightmost) {:stack-types [:zip]}))

(defn zip-tester
  [test-fn]
  (fn [state]
    (if (empty? (:zip state))
      state
      (let [result (ignore-errors (test-fn (top-item :zip state)))]
        (if (nil? result)
          state
          (push-item result :boolean (pop-item :zip state)))))))

(define-registered zip_end? (with-meta (zip-tester zip/end?) {:stack-types [:zip :boolean]}))
(define-registered zip_branch? (with-meta (zip-tester zip/branch?) {:stack-types [:zip :boolean]}))

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

(define-registered zip_replace_fromcode (with-meta (zip-inserter :code zip/replace) {:stack-types [:zip :code]}))
(define-registered zip_replace_fromexec (with-meta (zip-inserter :exec zip/replace) {:stack-types [:zip :exec] :parentheses 1}))

(define-registered zip_insert_right_fromcode (with-meta (zip-inserter :code zip/insert-right) {:stack-types [:zip :code]}))
(define-registered zip_insert_right_fromexec (with-meta (zip-inserter :exec zip/insert-right) {:stack-types [:zip :exec] :parentheses 1}))

(define-registered zip_insert_left_fromcode (with-meta (zip-inserter :code zip/insert-left) {:stack-types [:zip :code]}))
(define-registered zip_insert_left_fromexec (with-meta (zip-inserter :exec zip/insert-left) {:stack-types [:zip :exec] :parentheses 1}))

(define-registered zip_insert_child_fromcode (with-meta (zip-inserter :code zip/insert-child) {:stack-types [:zip :code]}))
(define-registered zip_insert_child_fromexec (with-meta (zip-inserter :exec zip/insert-child) {:stack-types [:zip :exec] :parentheses 1}))

(define-registered zip_append_child_fromcode (with-meta (zip-inserter :code zip/append-child) {:stack-types [:zip :code]}))
(define-registered zip_append_child_fromexec (with-meta (zip-inserter :exec zip/append-child) {:stack-types [:zip :exec] :parentheses 1}))

(define-registered 
  zip_remove
  ^{:stack-types [:zip]}
  (fn [state]
    (if (empty? (:zip state))
      state
      (let [result (ignore-errors (zip/remove (top-item :zip state)))]
        (if result
          (push-item result :zip (pop-item :zip state))
          state)))))

(define-registered 
  zip_fromcode
  ^{:stack-types [:zip :code]}
  (fn [state]
    (if (empty? (:code state))
      state
      (let [result (ignore-errors (seq-zip (top-item :code state)))]
        (if result
          (push-item result :zip (pop-item :code state))
          state)))))

(define-registered 
  zip_fromexec
  ^{:stack-types [:zip :exec]
    :parentheses 1}
  (fn [state]
    (if (empty? (:exec state))
      state
      (let [result (ignore-errors (seq-zip (top-item :exec state)))]
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

(define-registered code_fromzipnode (with-meta (zip-extractor :code zip/node) {:stack-types [:zip :code]}))
(define-registered exec_fromzipnode (with-meta (zip-extractor :exec zip/node) {:stack-types [:zip :exec]}))

(define-registered code_fromziproot (with-meta (zip-extractor :code zip/root) {:stack-types [:zip :code]}))
(define-registered exec_fromziproot (with-meta (zip-extractor :exec zip/root) {:stack-types [:zip :exec]}))

(define-registered code_fromzipchildren (with-meta (zip-extractor :code zip/children) {:stack-types [:zip :code]}))
(define-registered exec_fromzipchildren (with-meta (zip-extractor :exec zip/children) {:stack-types [:zip :exec]}))

(define-registered code_fromziplefts (with-meta (zip-extractor :code zip/lefts) {:stack-types [:zip :code]}))
(define-registered exec_fromziplefts (with-meta (zip-extractor :exec zip/lefts) {:stack-types [:zip :exec]}))

(define-registered code_fromziprights (with-meta (zip-extractor :code zip/rights) {:stack-types [:zip :code]}))
(define-registered exec_fromziprights (with-meta (zip-extractor :exec zip/rights) {:stack-types [:zip :exec]}))
