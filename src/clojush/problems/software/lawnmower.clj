;; lawnmower.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2010
;; Modifed by Anil Saini, aks@cs.umass.edu

(ns clojush.problems.software.lawnmower
 (:use clojush.pushgp.pushgp
      [clojush pushstate interpreter random util globals]
      clojush.instructions.tag
      clojure.math.numeric-tower
      ))



;;;;;;;;;;;;
;; Koza's lawnmower problem, described in Chapter 8 of Genetic Programming II:
;; Automatic Discovery of Reusable Programs, by John Koza, MIT Press, 1994.
;;
;; This example shows how to extend the core Clojush system with an additional
;; type/stack, without changing clojush.clj.
;;
;; A couple of top-level calls are provided, commented out, at the bottom;
;; uncomment one to run it.

;;;;;;;;;;;;
;; A few things must be done in the clojush namespace.
;(in-ns '[clojush pushstate interpreter random util globals])

;; Redefine push-types to include :vector_integer and then redefine the push state structure.
;(def push-types '(:exec :integer :float :code :boolean :auxiliary :tag :vector_integer))
;(define-push-state-structure)
;(define-push-state-record-type)

;; Redefine recognize-literal to support vector_integers of the form [row column

;(defn recognize-literal
;  "If thing is a literal, return its type -- otherwise return false."
;  [thing]
;  (cond (integer? thing) :integer
;    (number? thing) :float
;    (or (= thing true) (= thing false)) :boolean
;    (vector? thing) :vector_integer ;; just assume length is right
;    true false))
  
;;;;;;;;;;;;
;; Return to the lawnmower namespace
;(in-ns 'clojush.problems.software.lawnmower) 

;; Define standard stack instructions for the new vector_integer type.
;(define-registered vector_integer_pop (popper :vector_integer))
;(define-registered vector_integer_dup (duper :vector_integer))
;(define-registered vector_integer_swap (swapper :vector_integer))
;(define-registered vector_integer_rot (rotter :vector_integer))
;; Other possibilities: flush, eq, stackdepth, yank, yankdup, shove

;; Define Koza's v8a "vector addition mod 8" function. This is a modified v8a
;;   that takes the modulo with respect to the lawn size.
(define-registered v8a
  (fn [state] 
    (if (and (not (empty? (rest (:vector_integer state))))
	     (not (empty? (:auxiliary state))))
      (let [lawnstate (stack-ref :auxiliary 0 state)
            topvec (stack-ref :vector_integer 0 state)
            nxtvec (stack-ref :vector_integer 1 state)]
        (->> (pop-item :vector_integer state)
          (pop-item :vector_integer)
          (push-item (vector (mod (+ (first topvec) (first nxtvec)) (:max-row lawnstate))
                             (mod (+ (second topvec) (second nxtvec)) (:max-column lawnstate)))
                     :vector_integer)))
      state)))

; test
;(stks (run-push '(1 2 integer_add [1 2] [3 4] v88 
;                 vector_integer_dup [5 5] [-7 5] v88) 
;                (make-push-state)))

;;;;;;;;;;;;
;; Define lawn state and lawnmower problem support functions.

(defstruct lawn-state 
  :grid :mowed :row :column :orientation :turns :moves 
  :turns-limit :moves-limit)

(defn new-lawn-state
  "Returns a new  lawn-state initialized to unmowed."
  [lawn-x lawn-y limit]
  (struct-map lawn-state 
    :grid (vec (for [r (range lawn-y)]
                 (vec (for [c (range lawn-x)] 1))))
    :mowed #{} ;set
    :row 0 
    :column 0 
    :max-row lawn-y
    :max-column lawn-x
    :orientation :east
    :turns 0
    :moves 0
    :turns-limit limit
    :moves-limit limit))

(defn loc-ahead
  "Returns a [row column] vector for the location ahead of the mower in the given state."
  [state]
  [(mod (case (:orientation state)
          :south (inc (:row state)) 
          :north (dec (:row state))
          (:row state))
     (:max-row state))
   (mod (case (:orientation state)
          :east (inc (:column state))
          :west (dec (:column state))
          (:column state))
     (:max-column state))])

(defn left-in
  "Returns a copy of the given lawn-state with the mower having made a left turn."
  [state]
  (if (and (< (:turns state) (:turns-limit state))
        (< (:moves state) (:moves-limit state)))
    (-> state
      (assoc :orientation (get {:east :north, :north :west, :west :south, :south :east}
                            (:orientation state)))
      (assoc :turns (inc (:turns state))))
    state))

(defn mow-in
  "Returns a copy of the given lawn-state with the mower having moved one step forward."
  [state]
  (if (and (< (:turns state) (:turns-limit state))
        (< (:moves state) (:moves-limit state)))
    (let [[new-row new-column] (loc-ahead state)]
      (-> state
        (assoc :moves (inc (:moves state)))
        (assoc :row new-row)
        (assoc :column new-column)
        (assoc :mowed (if (= 1 (nth (nth (:grid state) new-row) new-column))
                        (conj (:mowed state) [new-row new-column])
                        (:mowed state)))))
    state))

;;;;;;;;;;;;
;; Define actual Push instructions for lawnmower functions.

(define-registered left 
  (fn [state]
    (if-not (empty? (:auxiliary state))
      (let [lawnstate (stack-ref :auxiliary 0 state)]
	(->> state
	     (pop-item :auxiliary)
	     (push-item (left-in lawnstate) :auxiliary)))
      state)))

(define-registered mow 
  (fn [state]
    (if-not (empty? (:auxiliary state))
      (let [lawnstate (stack-ref :auxiliary 0 state)]
	(->> state
	     (pop-item :auxiliary)
	     (push-item (mow-in lawnstate) :auxiliary)))
      state)))

(define-registered frog 
  (fn [state]
    (if-not (empty? (:auxiliary state))
      (let [lawnstate (stack-ref :auxiliary 0 state)]    
        (if (and (< (:turns lawnstate) (:turns-limit lawnstate))
                 (< (:moves lawnstate) (:moves-limit lawnstate))
                 (not (empty? (:vector_integer state))))
	         (let [[shift-row shift-column] (first (:vector_integer state))
                new-row (mod (+ (:row lawnstate) shift-row) 
                             (:max-row lawnstate))
                new-column (mod (+ (:column lawnstate) shift-column)
                                (:max-column lawnstate))
                new-lawnstate (assoc lawnstate
                                     :moves (inc (:moves lawnstate))
                                     :row new-row
                                     :column new-column
                                     :mowed (if (= 1 (nth (nth (:grid lawnstate) new-row) new-column))
                                              (conj (:mowed lawnstate) [new-row new-column])
                                              (:mowed lawnstate)))]
            (->> state
              (pop-item :vector_integer)
              (pop-item :auxiliary)
              (push-item new-lawnstate :auxiliary)))
          state))
      state)))

;;;;;;;;;;;;
;; Define a high level fitness function so code for runs is cleaner.

;(defn lawnmower-fitness
;  "Returns a fitness function for the lawnmower problem with a lawn of the
;  specified size (x and y) and the specified limit on numbers of turns and
;  moves."
;  [x y limit]
;  (fn [individual]
;    (assoc individual
 ;          :errors 100)))
;           (let [state (run-push (:program individual)
;                                (push-item (new-lawn-state x y limit) :auxiliary 
;                                                      (make-push-state)))
;                 final-state (top-item :auxiliary state)]
;             (if (= 1 1)
;               (abs (- (* x y) 
;                       4))
;               1000)))))



(defn lawnmower-fitness1
  "Returns a fitness function for the lawnmower problem with a lawn of the
  specified size (x and y) and the specified limit on numbers of turns and
  moves."
  [x y limit]
  (fn [individual]
    (assoc individual
           :errors
           (doall
             (for [input (range 1)]
               (let [state (run-push (:program individual) 
                                     (push-item (new-lawn-state x y limit) :auxiliary
                                                (make-push-state)))
                     top-struct (top-item :auxiliary state)]
                 (if (not (nil? (:mowed top-struct)))
                   (abs (- (* x y)
                           (count (:mowed top-struct))))
                        1000)))))))


;(count (:mowed final-state))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; code for actual runs

;; standard 8x8 lawnmower problem
;(pushgp
; :error-function (lawnmower-fitness 8 8 100)
; :atom-generators (list 'left 'mow 'v8a 'frog (fn [] [(rand-int 8) (rand-int 8)]))
; :mutation-probability 0.3
; :crossover-probability 0.3
; :simplification-probability 0.3
; :reproduction-simplifications 10
; :max-points 200
; :evalpush-limit 1000)

(def argmap
  {:error-function (lawnmower-fitness1 8 4 50)
   :atom-generators (list 'left 'mow 'v8a 'frog (fn [] [(rand-int 8) (rand-int 8)])
                          (tag-instruction-erc [:exec] 1000)
                          (tagged-instruction-erc 1000))
   :max-points 200
   ;:max-genome-size-in-initial-program 250
   :evalpush-limit 500
   ;:population-size 1000
   ;:max-generations 500
   :parent-selection :lexicase
   :genetic-operator-probabilities {:alternation  0.2
                                   :uniform-mutation 0.2
                                   :uniform-close-mutation 0.1
                                   [:alternation :uniform-mutation] 0.5
                                   }
   ;:alternation-rate 0.01
   ;:alignment-deviation 10
   ;:uniform-mutation-rate 0.01
   ;:problem-specific-report moving-average-report
   ;:problem-specific-initial-report moving-average-initial-report
   ;:report-simplifications 0
   ;:final-report-simplifications 5000
   ;:max-error 1000000000
  })

;(println (run-push '(tag_exec_818 ([7 1] frog mow [6 0] [6 0] frog mow frog tagged_391) tagged_525)
 ;                        (make-push-state)))

