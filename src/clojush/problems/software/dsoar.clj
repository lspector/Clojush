;; dsoar.clj
;; Anil Saini, aks@cs.umass.edu

;; This is a version of the "Dirt-Sensing, Obstacle-Avoiding Robot" (DSOAR) problem first
;; described in:
;;   Spector, L. 1996. Simultaneous Evolution of Programs and their Control Structures.
;;   In Advances in Genetic Programming 2, edited by P. Angeline and K. Kinnear, pp. 137-154.
;;   Cambridge, MA: MIT Press. http://helios.hampshire.edu/lspector/pubs/AiGP2-post-final-e.pdf

;; The earlier version was written by Brian Martin in 2010-2011.

(ns clojush.problems.software.dsoar
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower
        ))


;(in-ns 'clojush)

;(def push-types '(:exec :integer :float :code :boolean :auxiliary :tag :vector_integer))
;(define-push-state-structure)

;(defn recognize-literal
;  "If thing is a literal, return its type -- otherwise return false."
;  [thing]
;  (cond (integer? thing) :integer
;    (number? thing) :float
;    (or (= thing true) (= thing false)) :boolean
;    (vector? thing) :vector_integer ;; just assume length is right
;    true false))

;(in-ns 'examples.dsoar)

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

;(define-registered vector_integer_pop (popper :vector_integer))
;(define-registered vector_integer_dup (duper :vector_integer))
;(define-registered vector_integer_swap (swapper :vector_integer))
;(define-registered vector_integer_rot (rotter :vector_integer))

(define-registered v8a
  (fn [state] 
    (if (and (not (empty? (rest (:vector_integer state))))
	     (not (empty? (:auxiliary state))))
      (let [floorstate (stack-ref :auxiliary 0 state)
	    topvec (stack-ref :vector_integer 0 state)
            nxtvec (stack-ref :vector_integer 1 state)]
        (->> (pop-item :vector_integer state)
	     (pop-item :vector_integer)
	     (push-item [(mod (+ (first topvec) (first nxtvec)) (:max-row floorstate))
			 (mod (+ (second topvec) (second nxtvec)) (:max-column floorstate))]
			:vector_integer)))
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
		 (not (empty? (:vector_integer state))))
	  (let [[shift-row shift-column] (first (:vector_integer state))
		new-row (mod (+ (:row floorstate) shift-row) 
			     (:max-row floorstate))
		new-column (mod (+ (:column floorstate) shift-column)
				(:max-column floorstate))]
	    (if-not (obstacle? [new-row new-column] state)	   
	      (->> state
		   (pop-item :vector_integer)
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

(define-registered if-false
  (fn [state]
    (if-not (empty? (rest (:exec state)))
      (push-item (if false
                   (first (:exec state))
                   (first (rest (:exec state))))
        :exec
        (pop-item :boolean (pop-item :exec (pop-item :exec state))))
      state)))



(defn mopper-fitness
 "Returns a fitness function for the dsoar problem with a floor of the
  specified size (x and y) and the specified limit on numbers of turns and
  moves."
  [x y limit]
  (fn [individual]
    (let [num-obs-per-set (count (first (obstacles y)))]
      (assoc individual
             :errors
             (doall
               (for [i '(0 1)]
                 (let [state (run-push (:program individual) 
                                       (push-item (new-floor-state x y limit i) :auxiliary
                                                  (make-push-state)))
                       top-struct (top-item :auxiliary state)]
                   (if (not (nil? (:mowed top-struct)))
                     (abs (- (* x y)
                             (+ (count (:mowed top-struct)) num-obs-per-set)))
                          1000))))))))
     


(def argmap
  {:error-function (mopper-fitness 8 8 100)
   :atom-generators (list 'if-dirty 'if-obstacle 'left 'mop 'v8a 'frog (fn [] [(rand-int 8) (rand-int 8)])
                          (tag-instruction-erc [:exec] 1000)
                          (tagged-instruction-erc 1000)
                          )
   :max-points 1000
   ;:max-genome-size-in-initial-program 250
   :evalpush-limit 1000
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


#_(runner/run {:floor-dimensions [8 6], :limit 75, 
	     :instruction-set :basic,
	     :mutation-probability 0,
	     :crossover-probability 0,
	     :simplification-probability 0,
	     :self-composition-probability 0,
	     :other-composition-probability 0,
	     :self-other-composition-probability 0.9})