(ns clojush.pushgp.genetic-operators
  (:use [clojush.util]
        [clojush.random]
        [clojush.globals]
        [clojush.individual]
        [clojush.node-selection]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; genetic operators

(defn mutate 
  "Returns a mutated version of the given individual."
  [ind mutation-max-points max-points atom-generators]
  (let [new-program (insert-code-at-point (:program ind) 
                                          (select-node-index (:program ind))
                                          (random-code mutation-max-points atom-generators))]
    (if (> (count-points new-program) max-points)
      ind
      (make-individual :program new-program :history (:history ind)
                       :ancestors (if maintain-ancestors
                                    (cons (:program ind) (:ancestors ind))
                                    (:ancestors ind))))))

(defn crossover 
  "Returns a copy of parent1 with a random subprogram replaced with a random 
   subprogram of parent2."
  [parent1 parent2 max-points]
  (let [new-program (insert-code-at-point 
                      (:program parent1) 
                      (select-node-index (:program parent1))
                      (code-at-point (:program parent2)
                                     (select-node-index (:program parent2))))]
    (if (> (count-points new-program) max-points)
      parent1
      (make-individual :program new-program :history (:history parent1)
                       :ancestors (if maintain-ancestors
                                    (cons (:program parent1) (:ancestors parent1))
                                    (:ancestors parent1))))))

(defn boolean-gsxover 
  "Returns a child produced from parent1 and parent2 using boolean geometric
   semantic crossover. The child will be of the form:
   (new-random-code exec_if parent1-code parent2-code)."
  [parent1 parent2 new-code-max-points max-points atom-generators]
  (let [new-program (list (random-code new-code-max-points atom-generators) 
                          'exec_if 
                          (:program parent1) 
                          (:program parent2))]
    (if (> (count-points new-program) max-points)
      parent1
      (make-individual :program new-program :history (:history parent1)
                       :ancestors (if maintain-ancestors
                                    (cons (:program parent1) (:ancestors parent1))
                                    (:ancestors parent1))))))

(defn delete-mutate
  "Returns the individual with between 1 and 4 points deleted. The points can be
   single instructions or parenthetical pairs. The number of points is based
   roughly on a binomial distribution with n=4 and p=0.25, moved up one so that
   0 is never chosen. This results in the following probabilities for numbers
   of deletions:
   p(1) = 0.32
   p(2) = 0.42
   p(3) = 0.21
   p(4) = 0.05"
  [ind]
  (let [new-program (loop [prog (:program ind)
                           how-many (let [prob (lrand)]
                                      (cond
                                        (< prob 0.32) 1
                                        (< prob 0.74) 2
                                        (< prob 0.95) 3
                                        true 4))]
                      (if (zero? how-many)
                        prog
                        (recur (let [point-index (lrand-int (count-points prog))
                                     point (code-at-point prog point-index)]
                                 (if (and (seq? point)
                                          (< (lrand) 0.2))
                                   (remove-parens-at-point prog point-index)
                                   (remove-code-at-point prog point-index)))
                               (dec how-many))))]
    (make-individual :program new-program :history (:history ind)
                     :ancestors (if maintain-ancestors
                                  (cons (:program ind) (:ancestors ind))
                                  (:ancestors ind)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some utilities are required for gaussian mutation

(defn gaussian-noise-factor
  "Returns gaussian noise of mean 0, std dev 1."
  []
  (* (Math/sqrt (* -2.0 (Math/log (lrand))))
     (Math/cos (* 2.0 Math/PI (lrand)))))

(defn perturb-with-gaussian-noise 
  "Returns n perturbed with std dev sd."
  [sd n]
  (+' n (* sd (gaussian-noise-factor))))

(defn perturb-code-with-gaussian-noise
  "Returns code with each float literal perturbed with std dev sd and perturbation probability
   num-perturb-probability."
  [code per-num-perturb-probability sd]
  (postwalklist (fn [item]
                  (if (and (float? item)
                           (< (lrand) per-num-perturb-probability))
                    (perturb-with-gaussian-noise sd item)
                    item))
                code))

(defn gaussian-mutate 
  "Returns a gaussian-mutated version of the given individual."
  [ind per-num-perturb-probability sd]
  (make-individual 
    :program (perturb-code-with-gaussian-noise (:program ind) per-num-perturb-probability sd)
    :history (:history ind)
    :ancestors (if maintain-ancestors
                 (cons (:program ind) (:ancestors ind))
                 (:ancestors ind))))
