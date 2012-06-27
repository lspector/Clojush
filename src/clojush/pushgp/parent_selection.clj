(ns clojush.pushgp.parent_selection
  (:require [clojure.set :as set])
  (:use [clojush.random]
        [clojush.globals]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lexicase selection

(defn lexicase-selection
  "Returns an individual that does the best on a randomly selected set of fitness cases"
  [pop]
  (loop [survivors pop
         cases (shuffle (range (count (:errors (first pop)))))]
    (if (or (empty? cases)
            (empty? (rest survivors)))
      (lrand-nth survivors)
      (let [min-err-for-case (apply min (map #(nth % (first cases))
                                             (map #(:errors %) survivors)))]
        (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                       survivors)
               (rest cases))))))

(defn setup-fast-lexicase-selection
  "Sets global-lexicase-case-cohorts to be the data structure needed for fast-lexicase-selection.
   The value is a vector of copies of the population, one for each fitness case, with each 
   sorted by error on the specified case and then partitioned into sequences of individuals with the
   same fitness for that case."
  [pop]
  (print "Setting up for fast lexicase selection... ")(flush)
  (reset! global-lexicase-case-cohorts
          (vec (map (fn [case-index]
                      (partition-by 
                        #(nth (:errors %) case-index)
                        (sort #(< (nth (:errors %1) case-index) 
                                  (nth (:errors %2) case-index)) 
                              pop)))
                    (range (count (:errors (first pop)))))))
  ;; this prints a lot but it's a good sanity check and maybe useful information some day
  ;(println "Cohort sizes:" (map (fn [cohorts] (map count cohorts)) @global-lexicase-case-cohorts))
  (println "Done.")(flush)
  )

(defn fast-lexicase-selection
  "Returns an individual that does the best on a randomly selected set of fitness cases.
   Assumes that setup-fast-lexicase-selection has been run, setting up global-lexicase-case-cohorts."
  [pop]
  (loop [cases (lshuffle (range (count (:errors (first pop)))))
         survivors (first (nth @global-lexicase-case-cohorts (first cases)))]
    (if (or (empty? (rest cases))
            (empty? (rest survivors)))
      (lrand-nth survivors)
      (let [min-err-for-next-case (apply min (map #(nth % (second cases))
                                                  (map #(:errors %) survivors)))]
        (recur (rest cases)
               (filter #(= (nth (:errors %) (second cases)) min-err-for-next-case)
                       survivors))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parent selection

(defn select
  "Returns a selected parent, using lexicase or tournament selection."
  [pop tournament-size radius location]
  (if @global-use-fast-lexicase-selection
    (fast-lexicase-selection pop)
    (if @global-use-lexicase-selection
      (lexicase-selection pop)
      (let [tournament-set 
            (doall
              (for [_ (range tournament-size)]
                (nth pop
                     (if (zero? radius)
                       (lrand-int (count pop))
                       (mod (+ location (- (lrand-int (+ 1 (* radius 2))) radius))
                            (count pop))))))
            err-fn (if @global-use-historically-assessed-hardness :hah-error :total-error)]
        (reduce (fn [i1 i2] (if (< (err-fn i1) (err-fn i2)) i1 i2))
                tournament-set)))))
