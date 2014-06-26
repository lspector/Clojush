;; dsoar.clj

;; This is a version of the "Dirt-Sensing, Obstacle-Avoiding Robot" (DSOAR) problem first
;; described in:
;;   Spector, L. 1996. Simultaneous Evolution of Programs and their Control Structures.
;;   In Advances in Genetic Programming 2, edited by P. Angeline and K. Kinnear, pp. 137-154.
;;   Cambridge, MA: MIT Press. http://helios.hampshire.edu/lspector/pubs/AiGP2-post-final-e.pdf

;; This version was written by Brian Martin in 2010-2011.

(ns clojush.examples.dsoar
  (:use [clojush.pushgp.pushgp]
        [clojush.pushstate]
        [clojush.random]
        [clojush.interpreter]
        [clojure.math.numeric-tower]
        [clojush.instructions.common]
        [clojush.instructions.tag]))

(in-ns 'clojush.globals)
(def push-types '(:exec :integer :float :code :boolean :auxiliary :tag :intvec2D))

(in-ns 'clojush.pushstate)
(define-push-state-record-type)

(in-ns 'clojush.interpreter)
(defn recognize-literal
  "If thing is a literal, return its type -- otherwise return false."
  [thing]
  (cond (integer? thing) :integer
        (number? thing) :float
        (or (= thing true) (= thing false)) :boolean
        (vector? thing) :intvec2D ;; just assume length is right
        :else false))

(in-ns 'clojush.examples.dsoar)

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
    (if (and (not (empty? (rest (:intvec2D state))))
             (not (empty? (:auxiliary state))))
      (let [floorstate (stack-ref :auxiliary 0 state)
            topvec (stack-ref :intvec2D 0 state)
            nxtvec (stack-ref :intvec2D 1 state)]
        (->> (pop-item :intvec2D state)
          (pop-item :intvec2D)
          (push-item [(mod (+ (first topvec) (first nxtvec)) (:max-row floorstate))
                      (mod (+ (second topvec) (second nxtvec)) (:max-column floorstate))]
                     :intvec2D)))
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
  [floor-state]
  (if (and (< (:turns floor-state) (:turns-limit floor-state))
           (< (:moves floor-state) (:moves-limit floor-state)))
    (-> floor-state
        (assoc :orientation (get {:east :north, :north :west, :west :south, :south :east}
                                 (:orientation floor-state)))
        (assoc :turns (inc (:turns floor-state))))
    floor-state))

(defn mop-in
  "Returns a copy of the given floor-state with the mower having moved one step forward
   if it hasn't run into an obstacle."
  [floor-state]
  (let [[new-row new-column] (loc-ahead floor-state)]
    (if (and (< (:turns floor-state) (:turns-limit floor-state))
             (< (:moves floor-state) (:moves-limit floor-state))
             (not (obstacle? [new-row new-column] floor-state)))
      (-> floor-state
          (assoc :moves (inc (:moves floor-state)))
          (assoc :row new-row)
          (assoc :column new-column)
          (assoc :mopped (if (= 1 (nth (nth (:grid floor-state) new-row) new-column))
                           (conj (:mopped floor-state) [new-row new-column])
                           (:mopped floor-state))))
      floor-state)))

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
    (if-not (empty? (:auxiliary state))
      (let [floor-state (stack-ref :auxiliary 0 state)]
        (->> state
          (pop-item :auxiliary)
          (push-item (left-in floor-state) :auxiliary)))
      state)))

(define-registered mop
  (fn [state]
    (if-not (empty? (:auxiliary state))
      (let [floor-state (stack-ref :auxiliary 0 state)]
        (->> state
          (pop-item :auxiliary)
          (push-item (mop-in floor-state) :auxiliary)))
      state)))

(define-registered frog 
  (fn [state]
    (if-not (empty? (:auxiliary state))
      (let [floorstate (stack-ref :auxiliary 0 state)]    
        (if (and (< (:turns floorstate) (:turns-limit floorstate))
                 (< (:moves floorstate) (:moves-limit floorstate))
                 (not (empty? (:intvec2D state))))
          (let [[shift-row shift-column] (first (:intvec2D state))
                new-row (mod (+ (:row floorstate) shift-row) 
                             (:max-row floorstate))
                new-column (mod (+ (:column floorstate) shift-column)
                                (:max-column floorstate))]
            (if-not (obstacle? [new-row new-column] state)    
              (->> state
                (pop-item :intvec2D)
                (pop-item :auxiliary)
                (push-item (assoc floorstate
                                  :moves (inc (:moves floorstate))
                                  :row new-row
                                  :column new-column
                                  :mowed (if (= 1 (nth (nth (:grid floorstate) new-row) new-column))
                                           (conj (:mowed floorstate) [new-row new-column])
                                           (:mowed floorstate)))
                           :auxiliary))
              state))
          state))
      state)))

(define-registered if-obstacle
  (fn [state]
    (if-not (empty? (:auxiliary state))
      (if-obstacle-in state)
      state)))

(define-registered if-dirty
  (fn [state]
    (if-not (empty? (:auxiliary state))
      (if-dirty-in state)
      state)))

; Set paren requirements for new instructions
(swap! instr-paren-requirements assoc
       'if-dirty 2
       'if-obstacle 2)

(defn mopper-fitness
  "Returns a fitness function for the dsoar problem with a floor of the
   specified size (x and y) and the specified limit on numbers of turns and
   moves."
  [x y limit]
  (fn [program]
    (let [num-obs-per-set (count (first (obstacles y)))]
      (doall
        (map (partial - (* x y) num-obs-per-set)
             (for [i '(0 1)]
               (count
                 (:mopped
                   (first
                     (:auxiliary
                       (run-push program
                                 (push-item (new-floor-state x y limit i)
                                            :auxiliary (make-push-state)) ;true
                                 )))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; code for actual runs

;; standard 8x8 dsoar problem
#_(def argmap
  {:error-function (mopper-fitness 8 8 100)
   :atom-generators (list 'if-dirty 'if-obstacle 'left 'mop 'v8a 'frog
                          (fn [] [(lrand-int 8) (lrand-int 8)]))
   :mutation-probability 0.3
   :crossover-probability 0.3
   :simplification-probability 0.3
   :reproduction-simplifications 10
   :max-points 200
   :max-points-in-initial-program 200
   :evalpush-limit 1000
   })

;; standard 8x8 dsoar problem but with tags
(def argmap
  {:error-function (mopper-fitness 8 8 100)
   :atom-generators (list 'if-dirty 'if-obstacle 'left 'mop 'v8a 'frog
                          (fn [] [(lrand-int 8) (lrand-int 8)])
                          (tag-instruction-erc [:exec] 1000)
                          (tagged-instruction-erc 1000))
   :genetic-operator-probabilities {:reproduction 0.1
                                    :alternation 0.45
                                    [:uniform-mutation :uniform-close-mutation] 0.45}
   :parent-selection :tournament
   ;;;;;; Old genetic operator probabilities; should add back in simplification when implemented
   ;:mutation-probability 0.3
   ;:crossover-probability 0.3
   ;:simplification-probability 0.3
   ;:reproduction-probability 0.1
   ;:reproduction-simplifications 10
   :max-points 200
   :max-points-in-initial-program 200
   :evalpush-limit 1000
   })
