(ns clojush.simplification
  (:use [clojush util globals pushstate random individual evaluate]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-simplification

(defn remove-paren-pair
  "Removes one random pair of parens from a program. Cannot remove outermost pair."
  [program]
  (if (not (seq? program))
    program
    (let [open-close-seq (list-to-open-close-sequence program)
          number-parens (count (filter #(= :open %) open-close-seq))]
      (if (<= number-parens 1)
        program
        (let [pair-to-remove (inc (lrand-int (dec number-parens))) ;don't want first pair
              pair-removed-program (loop [open-count 0
                                          close-diff max-number-magnitude
                                          program-so-far []
                                          rest-program open-close-seq]
                                     (cond
                                       (nil? rest-program) (apply list program-so-far)
                                       (= (first rest-program) :open) (if (== open-count pair-to-remove)
                                                                        (recur (inc open-count)
                                                                               1
                                                                               program-so-far
                                                                               (next rest-program))
                                                                        (recur (inc open-count)
                                                                               (inc close-diff)
                                                                               (conj program-so-far (first rest-program))
                                                                               (next rest-program)))
                                       (= (first rest-program) :close) (if (zero? (dec close-diff))
                                                                         (recur open-count
                                                                                max-number-magnitude
                                                                                program-so-far
                                                                                (next rest-program))
                                                                         (recur open-count
                                                                                (dec close-diff)
                                                                                (conj program-so-far (first rest-program))
                                                                                (next rest-program)))
                                       :else (recur open-count
                                                    close-diff
                                                    (conj program-so-far (first rest-program))
                                                    (next rest-program))))]
          (open-close-sequence-to-list pair-removed-program))))))

(defn auto-simplify 
  "Auto-simplifies the provided individual."
  ([ind error-function steps print? progress-interval]
    (auto-simplify ind error-function steps print? progress-interval false))
  ([ind error-function steps print? progress-interval maintain-ancestors]
    (when print? (printf "\nAuto-simplifying with starting size: %s" (count-points (:program ind))))
    (loop [step 0 program (:program ind) errors (:errors ind) total-errors (:total-error ind)]
      (when (and print? 
                 (or (>= step steps)
                     (zero? (mod step progress-interval))))
        (printf "\nstep: %s\nprogram: %s\nerrors: %s\ntotal: %s\nsize: %s\n" 
                step (pr-str (not-lazy program)) (not-lazy errors) total-errors (count-points program))
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
                              (if (or (zero? how-many)
                                      (<= (count-points p) 1))
                                p
                                (recur (remove-code-at-point p (inc (lrand-int (dec (count-points p)))))
                                       (dec how-many))))
                            ;; remove single paren pair
                            (remove-paren-pair program))
              new-errors (error-function new-program)
              new-total-errors (compute-total-error new-errors)] ;simplification bases its decision on raw error; HAH-error could also be used here
          (if (= new-errors errors) ; only keep the simplified program if its error vector is the same as the original program's error vector
            (recur (inc step) new-program new-errors new-total-errors)
            (recur (inc step) program errors total-errors)))))))

(defn auto-simplify-from-program
  [p error-function steps print? progress-interval]
  (let [errs (error-function p)]
    (auto-simplify (make-individual :program p
                                    :errors errs
                                    :total-error (reduce + errs))
                   error-function
                   steps
                   print?
                   progress-interval)))
                                  