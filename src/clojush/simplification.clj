(ns clojush.simplification
  (:use [clojush.util]
        [clojush.globals]
        [clojush.pushstate]
        [clojush.random]
        [clojush.individual]
        [clojush.evaluate]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-simplification

(defn flatten-seqs
  "A version of flatten that only flattens nested seqs."
  [x]
  (filter (complement seq?)
          (rest (tree-seq seq? seq x))))

(defn auto-simplify 
  "Auto-simplifies the provided individual."
  [ind error-function steps print? progress-interval]
  (when print? (printf "\nAuto-simplifying with starting size: %s" (count-points (:program ind))))
  (loop [step 0 program (:program ind) errors (:errors ind) total-errors (:total-error ind)]
    (when (and print? 
               (or (>= step steps)
                   (zero? (mod step progress-interval))))
      (printf "\nstep: %s\nprogram: %s\nerrors: %s\ntotal: %s\nsize: %s\n" 
              step (not-lazy program) (not-lazy errors) total-errors (count-points program))
      (flush))
    (if (>= step steps)
      (make-individual :program program :errors errors :total-error total-errors 
                       :history (:history ind) 
                       :ancestors (if maintain-ancestors
                                    (cons (:program ind) (:ancestors ind))
                                    (:ancestors ind)))
      (let [new-program (if (< (lrand-int 5) 4)
                          ;; remove a small number of random things
                          (loop [p program how-many (inc (lrand-int 2))]
                            (if (zero? how-many)
                              p
                              (recur (remove-code-at-point p (lrand-int (count-points p)))
                                     (dec how-many))))
                          ;; flatten something
                          (let [point-index (lrand-int (count-points program))
                                point (code-at-point program point-index)]
                            (if (seq? point)
                              (insert-code-at-point program point-index (flatten-seqs point))
                              program)))
            new-errors (error-function new-program)
            new-total-errors (compute-total-error new-errors)] ;simplification bases its decision on raw error; HAH-error could also be used here
        (if (<= new-total-errors total-errors)
          (recur (inc step) new-program new-errors new-total-errors)
          (recur (inc step) program errors total-errors))))))
