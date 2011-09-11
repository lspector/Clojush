;; intertwined-spirals.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Kyle Harrington, kyleh@cs.brandeis.edu, 2010.

;; Edited by Lee Spector (lspector@hampshire.edu) to remove elements not yet 
;; merged into clojush master. Also commented out problem-specific-report,
;; which generates per-generation log files. Note that it is easy to try a
;; few different instruction sets by changing :basic to :exec or :tag in
;; intertwined-spirals-demo.
;; For Kyle's full version see https://github.com/kephale/Clojush

(ns examples.intertwined-spirals
  (:require [clojush] [clojure.contrib.duck-streams :as ds])
  (:use [clojush]))

(in-ns 'clojush)

(defn error-to-csv
  "Write a spiral classification to csv matrix file."
  [classification filename]
  (spit filename (reduce (fn [c d]
			   (str c "\n" d))
			 (map (fn [x]
				(reduce (fn [a b] (str a "," b)) x))
			      classification))))

#_(defn problem-specific-report
  "Customize this for your own problem. It will be called at the end of the generational report."
  [best population generation error-function report-simplifications]
  (error-to-csv (:classification (meta (error-function (:program best)))) (str "best_spiral_timeX" (java.util.Date.) "_genX" generation ".csv")))

(in-ns 'examples.intertwined-spirals)

;;;;;;;;;;;;
;; Intertwined Spirals problem
;; Introduced in "Learning to tell two spirals apart," Lang, K.J. and Witbrock, M.J., Proceedings of the 1988 Connectionist models summer school, 1988.
;; First solved with GP by John Koza in "A GENETIC APPROACH TO THE TRUCK BACKER UPPER PROBLEM AND THE INTER-TWINED SPIRAL PROBLEM,"  IJCNN, 1992.
;;

(def pi 3.1415)

(def num-samples 97)

(def solutions 
     (loop [cases '()
	    k 0]
       (if (>= k num-samples)
	 cases
	 (let [angle (* k pi (/ 1 16))
	       radius (* 6.5 (/ (- 104 k) 104))
	       x (* radius (Math/sin angle))
	       y (* radius (Math/cos angle))]
	   (recur (cons (list x y 1)
			(cons (list (* -1 x) (* -1 y) 0)
			      cases))
		  (inc k))))))
       
(define-registered x
  (fn [state] (push-item (stack-ref :auxiliary 0 state) :float state)))

(define-registered y
  (fn [state] (push-item (stack-ref :auxiliary 1 state) :float state)))

(define-registered iflte
  (fn [state]
    (if (and (not (empty? (rest (:float state))))
	     (not (empty? (rest (:exec state)))))
      (let [first-float (stack-ref :float 0 state)
            second-float (stack-ref :float 1 state)
	    first-exec (stack-ref :exec 0 state)
	    second-exec (stack-ref :exec 1 state)]	
        (->> (pop-item :float state)
	     (pop-item :float)
	     (pop-item :exec)
	     (pop-item :exec)	 
	     (push-item (if (<= second-float first-float)
			  first-exec
			  second-exec)
			:exec)))
      state)))

(defn classify-spiral
  "Return the classification of based on the current solution for belonging to 1 of 2 spirals.
Classification format: ( x, y, actual, prediction, correct? )
Prediction is spiral-1 if the (top float) > 0 and spiral-2 if the (top float) <= 0."
  [program]
  (doall 
   (for [k (range (count solutions))]
     (let [x (first (nth solutions k))
	   y (second (nth solutions k))
	   spiral (last (nth solutions k))
	   state (run-push program 
			   (push-item x :auxiliary (push-item y :auxiliary (make-push-state))))
	   top-float (top-item :float state)
	   invalid-output (or (not (number? top-float))
			      (= (:termination state) :abnormal))
	   predicted-spiral (cond invalid-output -1; Invalid flag
				  (> top-float 0) 1
				  :else           0)]
       (list x y spiral predicted-spiral (if (= spiral predicted-spiral) 1 0))))))

(def spiral-instructions 
  {:basic (list (fn [] (rand))
            'x 'y
            'float_add 'float_sub 'float_mult 'float_div
            'float_sin 'float_cos
            'iflte),
   :exec  (list (fn [] (rand))
            'x 'y
            'float_add 'float_sub 'float_mult 'float_div
            'float_sin 'float_cos
            'iflte 
            'exec_y 'exec_s 'exec_k 'exec_rot
            'exec_swap 'exec_dup 'exec_pop 'exec_eq),
   :tag (list (fn [] (rand))
          'x 'y
          'float_add 'float_sub 'float_mult 'float_div
          'float_sin 'float_cos
          'iflte
          (tag-instruction-erc [:float :exec] 100)
          (untag-instruction-erc 100)
          (tagged-instruction-erc 100))})		     

(defn spiral-error
  [program]
  (let [classification (classify-spiral program)]
    (with-meta (map #(cond (= (nth % 3) -1)  1000         ; Did we get an invalid reponse?
			   (= (last %) 1)       0 ; Correct answer?
			   :else               17); Else wrong
		    classification)
      {:classification classification})))

(defn intertwined-spirals-demo
  []
  (pushgp
   :trivial-geography-radius 17,
   :population-size 1000,
   :error-function spiral-error,
   :atom-generators (:basic spiral-instructions)))

(intertwined-spirals-demo)

