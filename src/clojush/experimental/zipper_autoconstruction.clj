;; Zipper-based Autoconstructive Evolution
;; by Kyle Harrington, 2011-2012.
;;
;; Note: experimental and under development.
;;
;; Harrington, K., E. Tosch, L. Spector, and J. Pollack, (2011)
;; "Compositional Autoconstructive Dynamics".
;; In Unifying Themes in Complex Systems Volume VIII: Proceedings of the Eighth International Conference on Complex Systems, pp. 856-870.
;; http://necsi.edu/events/iccs2011/papers/294.pdf
;;
;; Harrington, K. I., L. Spector, J. B. Pollack, and U.M. O'Reilly, (2012)
;; "Autoconstructive Evolution for Structural Problems".
;; In ECADA Workshop, Workshop Proceedings of the Genetic and Evolutionary Computation Conference (GECCO-2012), pp. 75-82.
;; http://pages.cs.brandeis.edu/~kyleh/autogecco2012.pdf

(ns clojush.experimental.zipper-autoconstruction
  (:use [clojush.globals]
        [clojush.pushstate]
	[clojush.util]
        [clojush.random]
	[clojush.interpreter]
        [clojush.instructions.boolean]
        [clojush.instructions.code]
        [clojush.instructions.common]
        [clojush.instructions.numbers]
        [clojush.instructions.random]
        [clojush.instructions.string]
        [clojush.instructions.tag]
        [clojush.instructions.zip]
        [clojush.individual]
        [clojush.evaluate]
	[clojush.node-selection]
        [clojush.pushgp.breed]
        [clojush.pushgp.parent-selection]
        [clojush.pushgp.report]
	[clojush.experimental.overlap])
  (:require [clojure.zip :as zip]))

(def maintain-parents-only true) ;; if true saves only the immediate parents of each individual

(def autoconstructive-atom-generators (list ;'exec_if
                                            'zip_down ;'zip_up
                                            'zip_left
                                            'zip_right ;'zip_rightmost
                                            ;'zip_shift_right 'zip_shift_left
                                            ;'zip_rotate_right 'zip_rotate_left
                                            ;'zip_subtree_dup
					    ;'zip_remove
                                            ;'autoconstructing
                                            ;'zip_reverse
                                            'zip_rand
                                            'zip_root
                                            'zip_rloc 'zip_rrloc))

(def recombinatory-autoconstructive-atom-generators (list 'zip_swap 'zip_subtree_swap))
#_(def recombinatory-autoconstructive-atom-generators (list 'zip_swap 'zip_subtree_swap
							  'zip_subtree_swap_bigger))

(defn num-els-stack
  "Returns the number of elements on a stack."
  [type state]
  (count (state type)))

(defn n-on-stack?
  "Boolean check if there are at least n elements on the specified stack."
  [n type state]
  (>= (num-els-stack type state) n))

(defn lt-n-on-stack?
  "Boolean check if there are fewer than n elements on the specified stack."
  [n type state]
  (< (num-els-stack type state) n))

;; Zipper manipulation instructions

(defn rcons
  [item col]
  (reverse (cons item (reverse col))))

(defn left-rotation
  [tree]
  (if (and (seq? tree)
	   (not (empty? tree))
	   (seq? (last tree))
	   (not (empty? (last tree))))
    (let [newroot (if (seq? (last tree)) (last tree) (list (last tree)))]
      (cons (rcons (first newroot)
		   (butlast tree))
	    (rest newroot)))
    tree))

(defn right-rotation
  [tree]
  (if (and (seq? tree)
	   (not (empty? tree))
	   (seq? (first tree))
	   (not (empty? (first tree))))
    (let [newroot (if (seq? (first tree)) (first tree) (list (first tree)))]
      (rcons (cons (last newroot)
		   (rest tree))
	     (butlast newroot)))
    tree))

(defn left-shift
  [tree]
  (if (and (seq? tree)
	   (not (empty? tree)))
    (rcons (first tree) (rest tree))
    tree))

(defn right-shift
  [tree]
  (if (and (seq? tree)
	   (not (empty? tree)))
    (cons (last tree) (butlast tree))
    tree))

(defn subtree-dup
  [tree]
  (list tree tree))

(defn subtree-reverse
  [tree]
  (reverse tree))

(defn zip-modifier
  [modifier-fn]
  (fn [state]
    (if (or (empty? (:zip state))
	    (not (seq? (zip/node (top-item :zip state))))
	    (empty? (zip/node (top-item :zip state))))
      state
      (let [new-zip-top (zip/replace (top-item :zip state)
				     (modifier-fn (zip/node (top-item :zip state))))]
	(if (> (count-points (zip/root new-zip-top)) @global-max-points-in-program)
	  state
	  (push-item new-zip-top
		     :zip (pop-item :zip state)))))))

(define-registered zip_rotate_left (zip-modifier left-rotation))
(define-registered zip_rotate_right (zip-modifier right-rotation))
(define-registered zip_shift_left (zip-modifier left-shift))
(define-registered zip_shift_right (zip-modifier right-shift))
(define-registered zip_subtree_dup (zip-modifier subtree-dup))
(define-registered zip_reverse (zip-modifier subtree-reverse))

;; Swap the subtrees at the location of the top 2 zip items
(define-registered zip_subtree_swap
  (fn [state]
    (if (n-on-stack? 2 :zip state)
      (let [first-zip (top-item :zip state)
	    second-zip (top-item :zip (pop-item :zip state))
	    new-first (zip/replace first-zip (zip/node second-zip))
	    new-second (zip/replace second-zip (zip/node first-zip))]
	(push-item new-second :zip
		   (push-item new-first :zip
			      (pop-item :zip
					(pop-item :zip state)))))
      state)))

(define-registered zip_subtree_swap_bigger
  (fn [state]
    (if (n-on-stack? 2 :zip state)
      (let [first-zip (top-item :zip state)
	    second-zip (top-item :zip (pop-item :zip state))
	    subtrees (sort-by count-points [(zip/node second-zip) (zip/node first-zip)])
	    new-first (zip/replace first-zip (last subtrees))
	    new-second (zip/replace second-zip (first subtrees))]
	(push-item new-second :zip
		   (push-item new-first :zip
			      (pop-item :zip
					(pop-item :zip state)))))
      state)))

(define-registered zip_subtree_swap_smaller
  (fn [state]
    (if (n-on-stack? 2 :zip state)
      (let [first-zip (top-item :zip state)
	    second-zip (top-item :zip (pop-item :zip state))
	    subtrees (sort-by count-points [(zip/node second-zip) (zip/node first-zip)])
	    new-first (zip/replace first-zip (first subtrees))
	    new-second (zip/replace second-zip (last subtrees))]
	(push-item new-second :zip
		   (push-item new-first :zip
			      (pop-item :zip
					(pop-item :zip state)))))
      state)))

(define-registered zip_subtree_from_second
  (fn [state]
    (if (n-on-stack? 2 :zip state)
      (let [first-zip (top-item :zip state)
	    second-zip (top-item :zip (pop-item :zip state))
	    new-first (zip/replace first-zip (zip/node second-zip))]
	(push-item second-zip :zip
		   (push-item new-first :zip
			      (pop-item :zip
					(pop-item :zip state)))))
      state)))

(define-registered zip_rloc
  (fn [state]
    (if (n-on-stack? 1 :zip state)
      (if (or (not (seq? (zip/node (top-item :zip state))))
              (empty? (zip/node (top-item :zip state))))
        state
        (let [top-zip (top-item :zip state)
              next-zip (loop [z top-zip
                              c (select-node-index (zip/node top-zip))]
                         (if (zero? c)
                           z
                           (recur (zip/next z) (dec c))))]
          (push-item next-zip :zip
                     (pop-item :zip state)))))))

(define-registered zip_rrloc
  (fn [state]
    (if (n-on-stack? 1 :zip state)
      (if (or (not (seq? (zip/node (top-item :zip state))))
              (empty? (zip/node (top-item :zip state))))
        state
        (let [top-zip (zip/seq-zip (zip/root (top-item :zip state)))
              next-zip (loop [z top-zip
                              c (select-node-index (zip/node top-zip))]
                         (if (zero? c)
                           z
                           (recur (zip/next z) (dec c))))]
          (push-item next-zip :zip
                     (pop-item :zip state)))))))

(define-registered zip_root
  (fn [state]
    (if (n-on-stack? 1 :zip state)
      (if (or (not (seq? (zip/node (top-item :zip state))))
              (empty? (zip/node (top-item :zip state))))
        state
	(push-item (zip/seq-zip (zip/root (top-item :zip state)))
		   :zip (pop-item :zip state))))))


(define-registered zip_rand
  (fn [state]
    (if (n-on-stack? 1 :zip state)
      (if (or (not (seq? (zip/node (top-item :zip state))))
              (empty? (zip/node (top-item :zip state))))
        state
	(if (empty? @clojush.globals/global-atom-generators)
	  (binding [*out* *err*]
	    (println "code_rand: global-atom-generators is empty.")
	    state)
	  (let [top-zip (top-item :zip state)
		zip-points (count-points (zip/root top-zip))
		max-subtree-points (- @global-max-points-in-program zip-points)]
	    (push-item (zip/replace top-zip
				    (random-code max-subtree-points
						 @global-atom-generators))
		       :zip (pop-item :zip state)))))
      state)))

(def global-ace-atom-generators
     (concat autoconstructive-atom-generators
	     recombinatory-autoconstructive-atom-generators))

(defn verify-composition
  "Verify that the program is valid."
  [composition-product max-points]
  (if (or (= :no-stack-item composition-product)
          (nil? composition-product))
    :no-program
    (let [program (zip/root composition-product)]
      (if (not (seq? program))
        :single-instruction
	(cond (> (count-points program) max-points)
	      :oversize
	      (= '() program)
	      :empty-program
	      (nil? program)
	      :nil-program
	      :else
	      program)))))

(define-registered autoconstructing
  (fn [state]
    (push-item (not (nil? (state :autoconstructing))) :boolean state)))

(defn autoconstruct
  "Get the product of zipper autoconstruction."
  [autoconstruction-program max-points]
  (let [state (run-push autoconstruction-program (assoc (make-push-state)
                                                   :autoconstructing true)
			)]
    (verify-composition (top-item :zip state) max-points)))

(defn autoconstructive-mutate
  "Returns an unvaried child with incremented ancestry."
  [max-points parent1 parent2]
  (let [composition-program (concat (cons 'zip_fromexec (list (:program parent2)))
				    (filter #(not (number? %)) (ensure-list (:program parent1))))
	product (autoconstruct composition-program max-points)]
    (if (nil? product)
      parent1
      (make-individual :program product :history (:history parent1)
		       :ancestors (if maintain-ancestors
				    (if maintain-parents-only
				      [(:program parent1) (:program parent2)]
				      (cons (:program parent1) (:ancestors parent1)))
				    (:ancestors parent1))))))

(defn autoconstructive-crossover
  "Returns an unvaried child with incremented ancestry."
  [max-points parent1 parent2]
  (let [composition-program (concat (cons 'zip_fromexec (list (:program parent1)))
				    (cons 'zip_fromexec (list (:program parent2)))
				    (filter #(not (number? %)) (ensure-list (:program parent1))))
	product (autoconstruct composition-program max-points)]
    (if (nil? product)
      parent1
      (make-individual :program product :history (:history parent1)
		       :ancestors (if maintain-ancestors
				    (if maintain-parents-only
				      [(:program parent1) (:program parent2)]
				      (cons (:program parent1) (:ancestors parent1)))
				    (:ancestors parent1))))))


