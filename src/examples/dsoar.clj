;; dsoar.clj

;; This is a version of the "Dirt-Sensing, Obstacle-Avoiding Robot" (DSOAR) problem first
;; described in:
;;   Spector, L. 1996. Simultaneous Evolution of Programs and their Control Structures.
;;   In Advances in Genetic Programming 2, edited by P. Angeline and K. Kinnear, pp. 137-154.
;;   Cambridge, MA: MIT Press. http://helios.hampshire.edu/lspector/pubs/AiGP2-post-final-e.pdf

;; This version was written by Brian Martin in 2010-2011.


(ns examples.dsoar
  (:require [clojush] [clojure.contrib.math])
  (:use [clojush] [clojure.contrib.math]))

(in-ns 'clojush)

(def push-types '(:exec :integer :float :code :boolean :auxiliary :tag :intvec2D))
(define-push-state-structure)

(defn recognize-literal
  "If thing is a literal, return its type -- otherwise return false."
  [thing]
  (cond (integer? thing) :integer
    (number? thing) :float
    (or (= thing true) (= thing false)) :boolean
    (vector? thing) :intvec2D ;; just assume length is right
    true false))

(in-ns 'examples.dsoar)

;the obstacles, created with:
#_(apply hash-map (interleave (filter even? (range 4 13))
  (for [i (filter even? (range 4 13))]
    (for [j (range 2)]
      (let [num-obstacles (floor (sqrt (* 8 i)))]
        (loop [coords #{}]
          (if (< (count coords) num-obstacles)
            (recur (conj coords [(rand-int i) (rand-int 8)]))
            coords)))))))

(def obstacles {4  [#{[2 2] [1 1] [1 3] [2 5] [2 6]}
                    #{[2 3] [1 3] [0 3] [0 4] [3 0]}],
                6  [#{[0 0] [3 4] [4 7] [0 3] [0 5] [2 0]}
                    #{[5 5] [3 4] [0 2] [4 7] [1 7] [4 0]}],
                8  [#{[3 4] [5 6] [6 7] [3 6] [1 4] [7 0] [6 3] [5 3]}
                    #{[3 3] [5 6] [2 4] [0 3] [2 6] [0 4] [7 0] [7 2]}],
                10 [#{[0 0] [0 3] [0 5] [1 7] [7 0] [9 2] [8 2] [9 4]}
                    #{[6 5] [1 0] [2 2] [0 2] [1 6] [6 3] [8 5] [8 6]}],
                12 [#{[5 6] [0 2] [0 5] [9 0] [8 0] [10 2] [9 5] [10 6] [7 4]}
                    #{[6 6] [1 4] [0 4] [9 1] [7 0] [8 3] [11 6] [5 1] [6 4]}]})


(define-registered intvec2D_pop (popper :intvec2D))
(define-registered intvec2D_dup (duper :intvec2D))
(define-registered intvec2D_swap (swapper :intvec2D))
(define-registered intvec2D_rot (rotter :intvec2D))

(define-registered v8a
  (fn [state]
    (if (not (empty? (rest (:intvec2D state))))
      (let [topvec (stack-ref :intvec2D 0 state)
            nxtvec (stack-ref :intvec2D 1 state)]
        (->> (pop-item :intvec2D state)
          (pop-item :intvec2D)
          (push-item [(mod (+ (first topvec) (first nxtvec)) 8)
                      (mod (+ (second topvec) (second nxtvec)) 8)]
            :intVec2D)))
      state)))

(defstruct floor-state
  :grid :mopped :obstacles :row :column :orientation :turns :moves
  :turns-limit :moves-limit)

(defn new-floor-state
  "Returns a new  floor-state initialized to unmopped with obstacles."
  [floor-x floor-y limit obstacle-idx]
  (struct-map floor-state
    :grid (vec (for [r (range floor-y)]
                 (vec (for [c (range floor-x)] 1))))
    :mopped #{}
    :obstacles (nth (obstacles floor-y) obstacle-idx)
    :row 0
    :column 0
    :max-row floor-y
    :max-column floor-x
    :orientation :east
    :turns 0
    :moves 0
    :turns-limit limit
    :moves-limit limit))

(defn loc-ahead
  "Returns a [row column] vector for the location ahead of the mopper in the given state."
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

(defn obstacle?
  [loc state]
  (contains? (:obstacles state) loc))

(defn dirty?
  [loc state]
  (not (or (contains? (:mopped state) loc)
           (obstacle? loc state))))

(defn left-in
  "Returns a copy of the given floor-state with the mopper having made a left turn."
  [state]
  (if (and (< (:turns state) (:turns-limit state))
        (< (:moves state) (:moves-limit state)))
    (-> state
      (assoc :orientation (get {:east :north, :north :west, :west :south, :south :east}
                            (:orientation state)))
      (assoc :turns (inc (:turns state))))
    state))

(defn mop-in
  "Returns a copy of the given floor-state with the mower having moved one step forward
  if it hasn't run into an obstacle."
  [state]
  (let [[new-row new-column] (loc-ahead state)]
    (if (and (< (:turns state) (:turns-limit state))
             (< (:moves state) (:moves-limit state))
             (not (obstacle? [new-row new-column] state)))
      (-> state
        (assoc :moves (inc (:moves state)))
        (assoc :row new-row)
        (assoc :column new-column)
        (assoc :mopped (if (= 1 (nth (nth (:grid state) new-row) new-column))
                        (conj (:mopped state) [new-row new-column])
                        (:mopped state))))
      state)))

(defn frog-in
  "Returns a copy of the given floor-state with the mopper having frogged
to the location indicated by the top intvec2D (unless that location is an
obstacle."
  [state]
  (let [[new-row new-column] (first (:intvec2D state))]
    (if (and (< (:turns state) (:turns-limit state))
             (< (:moves state) (:moves-limit state))
             (not (empty? (:intvec2D state)))
             (not (obstacle? [new-row new-column] state)))
        (-> state
          (pop-item :intvec2D)
          (assoc :moves (inc (:moves state)))
          (assoc :row new-row)
          (assoc :column new-column)
          (assoc :mopped (if (= 1 (nth (nth (:grid state) new-row) new-column))
                          (conj (:mopped state) [new-row new-column])
                          (:mopped state))))
        state)))

(defn if-obstacle-in
  [push-state]
  (let [floor-state (top-item :auxiliary push-state)
        A (top-item :exec push-state)
        B (top-item :exec (pop-item :exec push-state))]
    (if (and (not= :no-stack-item A)
             (not= :no-stack-item B))
      (if (obstacle? (loc-ahead floor-state) floor-state)
        (->> push-state (pop-item :exec)
                        (pop-item :exec)
                        (push-item A :exec))
        (pop-item :exec push-state))
      push-state)))

(defn if-dirty-in
  [push-state]
  (let [floor-state (top-item :auxiliary push-state)
        A (top-item :exec push-state)
        B (top-item :exec (pop-item :exec push-state))]
    (if (and (not= :no-stack-item A)
             (not= :no-stack-item B))
      (if (dirty? (loc-ahead floor-state) floor-state)
        (->> push-state (pop-item :exec)
                   (pop-item :exec)
                   (push-item A :exec))
        (pop-item :exec push-state))
      push-state)))

(define-registered left
  (fn [state]
    (let [floor-state (stack-ref :auxiliary 0 state)]
       (->> state
         (pop-item :auxiliary)
         (push-item (left-in floor-state) :auxiliary)))))

(define-registered mop
  (fn [state]
    (let [floor-state (stack-ref :auxiliary 0 state)]
       (->> state
         (pop-item :auxiliary)
         (push-item (mop-in floor-state) :auxiliary)))))

(define-registered frog
  (fn [state]
    (let [floor-state (stack-ref :auxiliary 0 state)]
       (->> state
         (pop-item :auxiliary)
         (push-item (frog-in floor-state) :auxiliary)))))

(define-registered if-obstacle
  (fn [push-state]
    (if-obstacle-in push-state)))

(define-registered if-dirty
  (fn [push-state]
    (if-dirty-in push-state)))

(defn mopper-fitness
  "Returns a fitness function for the dsoar problem with a floor of the
  specified size (x and y) and the specified limit on numbers of turns and
  moves."
  [x y limit]
  (fn [program]
    (let [num-obs-per-set (count (first (obstacles y)))]
;      (println
;        (first
;          (:auxiliary
;            (run-push program
;              (push-item (new-floor-state x y limit 1)
;                :auxiliary (make-push-state))))))
      (doall
        (map (partial - (* x y) num-obs-per-set)
          (for [i '(0 1)]
            (count
              (:mopped
                (first
                  (:auxiliary
                    (run-push program
                      (push-item (new-floor-state x y limit i)
                        :auxiliary (make-push-state)))))))))))))

(defn run [params]
  (let [[floor-rows floor-cols] (params :lawn-dimensions)
        limit (params :limit)
        instruction-set (params :instruction-set)
        node-selection (params :node-selection)
        tournament-size (params :tournament-size)]
    (pushgp
      :error-function (mopper-fitness floor-rows floor-cols limit)
      :atom-generators (cond (= :basic instruction-set)
                                 (list 'if-dirty 'if-obstacle 'left 'mop 'v8a 'frog (fn [] [(rand-int 8) (rand-int 8)]))
                             (= :exec instruction-set)
                                 (list 'if-dirty 'if-obstacle 'left 'mop 'v8a 'frog (fn [] [(rand-int 8) (rand-int 8)])
                                       'exec_dup 'exec_pop 'exec_rot 'exec_swap 'exec_k 'exec_s 'exec_y)
                             (= :tag instruction-set)
                                 (list 'if-dirty 'if-obstacle 'left 'mop 'v8a 'frog (fn [] [(rand-int 8) (rand-int 8)])
                                       (tag-instruction-erc [:exec] 1000)
                                       (tagged-instruction-erc 1000)))
      :mutation-probability 0.45
      :crossover-probability 0.45
      :node-selection-method (if node-selection node-selection :unbiased)
      :node-selection-tournament-size tournament-size
      :simplification-probability 0.0
      :reproduction-simplifications 10
      :max-points (* 10 limit)
      :evalpush-limit (* 10 limit))))

(run {:lawn-dimensions [8 8] 
      :limit 100 
      :instruction-set :tag 
      :node-selection :unbiased
      :tournament-size 5})

#'examples.dsoar/run
