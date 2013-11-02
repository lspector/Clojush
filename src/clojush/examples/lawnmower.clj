;; lawnmower.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2010

(ns clojush.examples.lawnmower
  (:use [clojush.pushgp.pushgp]
        [clojush.pushstate]
        [clojush.interpreter]
        [clojush.globals]
        [cloush.random]
        [clojush.instructions.common]
        [clojush.instructions.tag]))

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
;; A few things must be done in other namespaces

;; Redefine push-types to include :intvec2D and then redefine the push state structure.
(in-ns 'clojush.globals)
(def push-types '(:exec :integer :float :code :boolean :auxiliary :tag :intvec2D))

(in-ns 'clojush.pushstate)
(define-push-state-record-type)

;; Redefine recognize-literal to support intvec2Ds of the form [row column]
(in-ns 'clojush.interpreter)
(defn recognize-literal
  "If thing is a literal, return its type -- otherwise return false."
  [thing]
  (cond (integer? thing) :integer
        (number? thing) :float
        (or (= thing true) (= thing false)) :boolean
        (vector? thing) :intvec2D ;; just assume length is right
        true false))
  
;;;;;;;;;;;;
;; Return to the lawnmower namespace
(in-ns 'clojush.examples.lawnmower) 

;; Define standard stack instructions for the new intvec2D type.
(define-registered intvec2D_pop (popper :intvec2D))
(define-registered intvec2D_dup (duper :intvec2D))
(define-registered intvec2D_swap (swapper :intvec2D))
(define-registered intvec2D_rot (rotter :intvec2D))
;; Other possibilities: flush, eq, stackdepth, yank, yankdup, shove

;; Define Koza's v8a "vector addition mod 8" function. This is a modified v8a
;;   that takes the modulo with respect to the lawn size.
(define-registered v8a
                   (fn [state] 
                     (if (and (not (empty? (rest (:intvec2D state))))
                              (not (empty? (:auxiliary state))))
                       (let [lawnstate (stack-ref :auxiliary 0 state)
                             topvec (stack-ref :intvec2D 0 state)
                             nxtvec (stack-ref :intvec2D 1 state)]
                         (->> (pop-item :intvec2D state)
                              (pop-item :intvec2D)
                              (push-item [(mod (+ (first topvec) (first nxtvec)) (:max-row lawnstate))
                                          (mod (+ (second topvec) (second nxtvec)) (:max-column lawnstate))]
                                         :intvec2D)))
                       state)))

; test
#_(println (run-push '(1 2 integer_add [1 2] [3 4] v8a 
                         intvec2D_dup [5 5] [-7 5] v8a) 
                     (make-push-state)))

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
              :mowed #{} 
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
                                  (not (empty? (:intvec2D state))))
                           (let [[shift-row shift-column] (first (:intvec2D state))
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
                                  (pop-item :intvec2D)
                                  (pop-item :auxiliary)
                                  (push-item new-lawnstate :auxiliary)))
                           state))
                       state)))

;;;;;;;;;;;;
;; Define a high level fitness function so code for runs is cleaner.

(defn lawnmower-fitness
  "Returns a fitness function for the lawnmower problem with a lawn of the
   specified size (x and y) and the specified limit on numbers of turns and
   moves."
  [x y limit]
  (fn [program]
    (doall
      (list (- (* x y)
               (count
                 (:mowed 
                   (first 
                     (:auxiliary
                       (run-push program 
                                 (push-item (new-lawn-state x y limit) 
                                            :auxiliary (make-push-state)) ;true
                                 ))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; code for actual runs

;; standard 8x8 lawnmower problem
#_(def argmap
  {:error-function (lawnmower-fitness 8 8 100)
   :atom-generators (list 'left 'mow 'v8a 'frog (fn [] [(lrand-int 8) (lrand-int 8)]))
   :mutation-probability 0.3
   :crossover-probability 0.3
   :simplification-probability 0.3
   :reproduction-simplifications 10
   :max-points 200
   :max-points-in-initial-program 200
   :evalpush-limit 1000
   })

;; standard 8x8 lawnmower problem but with tags
(def argmap
  {:error-function (lawnmower-fitness 8 8 100)
   :atom-generators (list 'left 'mow 'v8a 'frog (fn [] [(lrand-int 8) (lrand-int 8)])
                          (tag-instruction-erc [:exec] 1000)
                          (tagged-instruction-erc 1000))
   :mutation-probability 0.3
   :crossover-probability 0.3
   :simplification-probability 0.3
   :reproduction-probability 0.1
   :reproduction-simplifications 10
   :max-points 200
   :max-points-in-initial-program 200
   :evalpush-limit 1000
   })
