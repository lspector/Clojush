;; modularity.clj
;;
;; Preliminary code for analyzing modularity in Clojush
;;
;; Copyright (c) 2011 Kyle Harrington (kyleh@cs.brandeis.edu)
;;
;; This program is free software: you can redistribute it and/or modify it under
;; the terms of version 3 of the GNU General Public License as published by the
;; Free Software Foundation, available from http://www.gnu.org/licenses/gpl.txt.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
;; PARTICULAR PURPOSE. See the GNU General Public License (http://www.gnu.org/licenses/)
;; for more details.

(ns analysis.modularity
  (:gen-class)
  (:require [clojure.contrib.string :as string])
  (:use [clojush]))

;;;;;;;;;;;;;;; Lawnmower

(use 'examples.lawnmower)

(def solutions-8x8
     {:basic '([6 4] ([0 7] (mow (mow)) (mow)) (left v8a v8a left) (((v8a) ((left) mow) (mow mow) (frog mow (mow left)) (left left))) ((mow 7 4 2) left (left) (mow) (mow mow) left ((left)) (mow left frog mow left left 4 left left left mow mow left left v8a left left left frog left 4 7 2)) (left (((mow left mow mow mow mow mow mow mow left 5 7 mow mow mow mow mow mow mow 0 left frog mow mow left mow mow left left mow mow left left frog left mow left left left left mow mow) (mow 5 left left left mow v8a left mow left mow mow 4 mow left 2 left left mow left frog 4 6 v8a mow mow left 3)) v8a ((left mow (left mow left)) (((left)) (v8a) mow left) (mow) ((left mow mow) (frog) [5 5]) (5 mow mow mow mow left mow)) (left) (left (left) (v8a)) ((mow (left mow)) (left)) mow ((((mow) [5 0]) left ((frog) ((mow) left))) ((left v8a) mow) mow (frog mow (mow mow left 7 left left v8a) (left mow ((mow) (v8a mow mow 1 0 left mow mow 3 frog left))) (mow left left)))) (((mow mow mow)) mow left) (mow (mow) (mow left mow left 7 1 mow frog left 0 1 left mow v8a 5 4 mow mow frog) ((left)))) ((mow ([0 1] v8a ((left) mow)) (6 5 mow) (mow ((v8a) (left)) (left))) ((left)) (mow frog)))
      :exec '(((((exec_dup) (((mow) (left mow) (((exec_swap exec_rot (exec_swap exec_s left exec_dup)) exec_s) (left mow mow)) (exec_dup mow)) exec_y)))))
      :tag '((((((tagged_781) (v8a (((mow) ((((mow 7 0 mow) ((mow left)) [6 5]) (frog) (2 left)) v8a) mow)) (left ((mow left 1 mow) mow) (frog) left (frog mow)) (left) (left ((tag_exec_763) mow ((mow) mow)) tag_exec_6 mow (mow (tagged_598 v8a tagged_313) mow) (tag_exec_929 (tagged_397) ((left tag_exec_309))) (tagged_636)))) (tagged_81 mow frog mow tag_exec_326 left tagged_313)) ([6 7]) tagged_742 tagged_74) left (((tagged_541 (((mow) (((tagged_985) mow (((tagged_10) v8a) (mow tagged_307 left tagged_187 tagged_787 v8a tag_exec_357 tag_exec_256 mow left mow left left mow mow mow 2 tagged_56 mow) ((frog))) (mow mow left tag_exec_348 (left))) ((left left left v8a tag_exec_89) mow (mow (left v8a)) frog tag_exec_678 ((tag_exec_730))) mow (mow) ([3 3] left (((v8a) tag_exec_116) tagged_671 (mow tag_exec_774) v8a) tagged_416) (mow 7 v8a) ((tagged_19) tagged_910 ([2 3]) tagged_196)) mow (left frog ([3 5] (mow) left) ((((frog) left) mow) mow tagged_647)) (mow) ((left))) (left (v8a (mow (tag_exec_597)) (v8a) (left (mow v8a mow))) ((mow) frog) (((mow mow mow left tagged_306 1 mow mow left frog tagged_887 v8a) mow (tagged_536) tag_exec_758 ((([5 4])) (v8a))) tagged_469 left ((mow) left (left)) (tag_exec_938 (mow) (mow)) (([1 7] (tagged_618 tagged_661)) (tag_exec_190) tag_exec_481 (left v8a left tagged_272 tagged_511 6 7 tag_exec_757 tag_exec_392) ((tagged_672 [3 0]) left)) ((((tagged_95) (mow) (mow ((left)))) (((tagged_367)) left tag_exec_806 (mow) (left tagged_850 (mow mow mow left 5 tagged_737 frog 6 5 tag_exec_694 2 left) v8a) mow) tagged_951 (((v8a (tagged_469 tagged_292) mow) (mow) tag_exec_916 ([7 4]))) (left (((tagged_862))) ((mow (tag_exec_282) mow mow)))) (v8a)))) mow)) ((v8a))))) left tagged_925)})

(reset! global-evalpush-limit 1000)
(reset! global-evalpush-time-limit 0)

(def error-s1 (lawnmower-fitness 8 8 100))

(defn lawnmower-push-state
  [x y limit program]
  (let [state (run-push program 
			(push-item (new-lawn-state x y limit) 
				   :auxiliary (make-push-state)))]
    state))

(def solution-state
     (zipmap (keys solutions-8x8)
	     (doall (for [k (keys solutions-8x8)]
		      (lawnmower-push-state 8 8 100 (solutions-8x8 k))))))

(defn reuse
  [program state]
  (/ (count (:trace state))
     (count-points program)))

(defn modules
  [program]
  (let [ai (all-items program)]
    ai))

(defn trace-reuse
  [state]
  (frequencies (all-items (:trace state))))

(doall (for [k (keys solutions-8x8)]
	 (do ;(println (str k " = " (solutions-8x8 k)))
	     (println (str k))
	     (println (str "Error = " (error-s1 (solutions-8x8 k))))
	     (println (str "Reuse = " (float (reuse (solutions-8x8 k) (solution-state k)))))
	     (println (trace-reuse (solution-state k)))
	     (println ""))))


;;;;; Exception tests

(def s1 '([6 4] ([0 7] (mow (mow)) (mow)) (left v8a v8a left) (((v8a) ((left) mow) (mow mow) (frog mow (mow left)) (left left))) ((mow 7 4 2) left (left) (mow) (mow mow) left ((left)) (mow left frog mow left left 4 left left left mow mow left left v8a left left left frog left 4 7 2)) (left (((mow left mow mow mow mow mow mow mow left 5 7 mow mow mow mow mow mow mow 0 left frog mow mow left mow mow left left mow mow left left frog left mow left left left left mow mow) (mow 5 left left left mow v8a left mow left mow mow 4 mow left 2 left left mow left frog 4 6 v8a mow mow left 3)) v8a ((left mow (left mow left)) (((left)) (v8a) mow left) (mow) ((left mow mow) (frog) [5 5]) (5 mow mow mow mow left mow)) (left) (left (left) (v8a)) ((mow (left mow)) (left)) mow ((((mow) [5 0]) left ((frog) ((mow) left))) ((left v8a) mow) mow (frog mow (mow mow left 7 left left v8a) (left mow ((mow) (v8a mow mow 1 0 left mow mow 3 frog left))) (mow left left)))) (((mow mow mow)) mow left) (mow (mow) (mow left mow left 7 1 mow frog left 0 1 left mow v8a 5 4 mow mow frog) ((left)))) ((mow ([0 1] v8a ((left) mow)) (6 5 mow) (mow ((v8a) (left)) (left))) ((left)) (mow frog))))

(reset! global-evalpush-limit 1000)
(reset! global-evalpush-time-limit 0)

(def error-s1 (lawnmower-fitness 8 8 100))


(println (doall (for [_ (range 100)]
	 (error-s1 s1))))

(reset! global-evalpush-time-limit 1000000)
(println (doall (for [_ (range 100)]
		  (try (error-s1 s1)
		       (catch Exception e (println "caught"))
		       (finally)))))


;;;;;;;;;;;;;;;;;; DSOAR

