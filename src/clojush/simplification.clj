(ns clojush.simplification
  (:use [clojush util globals pushstate random individual evaluate translate]))

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
                                      (:ancestors ind))
                         :genetic-operators :simplification)
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
                                    :total-error (reduce + errs)
                                    :genetic-operators :simplification)
                   error-function
                   steps
                   print?
                   progress-interval)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-simplification of Plush genomes

(defn select-random-weighted-item
  [prob-map]
  (let [summed-probs (reductions (fn [[_ prev-prob] [item prob]]
                                   (vector item (+ prev-prob prob)))
                                 prob-map)
        rand-num (rand (second (last summed-probs)))]
    (loop [items-and-probs summed-probs]
      (if (< rand-num (second (first items-and-probs)))
        (first (first items-and-probs))
        (recur (rest items-and-probs))))))

(defn apply-simplification-step-to-genome
  [genome simplification-step-probabilities]
  (let [transformation-map (select-random-weighted-item simplification-step-probabilities)
        silencings (get transformation-map :silence 0)
        unsilencings (get transformation-map :unsilence 0)
        no-opings (get transformation-map :no-op 0)
        silent-values (map :silent genome)
        indices-available-to-silence (if (> silencings 0)
                                       (map second
                                            (filter #(not= (first %) true)
                                                    (map vector silent-values (range))))
                                       nil)
        indices-available-to-unsilence (if (> unsilencings 0) ;this isn't correct
                                         (map second
                                              (filter #(not= (first %) false)
                                                      (map vector silent-values (range))))
                                         nil)
        indices-available-to-no-op (if (> no-opings 0) ;this isn't correct
                                     (map second
                                          (filter #(not= (first %) :no-op)
                                                  (map vector silent-values (range))))
                                     nil)]
    (println indices-available-to-unsilence)
    genome
    ;;;MAKE CHANGES HERE
    ))

(defn auto-simplify-plush
  "Automatically simplifies the genome of an individual without changing its error vector on
   the training set, based on the error-function. steps is the number of hill-climbing evaluations
   to test. print-progress-interval is how often to print progress of the simplification; if it is
   set to 0, then nothing will be printed.
   simplification-step-probabilities is a map of probabilities that are used to select what change
   to make during each step of the simplification. Each change is represented as a map with the
   following options for the keys, each of which has an integer of how many of those changes to make:
     :silence - number of unsilenced or no-op genes to set :silent = true
     :unsilence - number of silenced or no-op genes to set :silent = false
     :no-op - number of unsilenced or silenced genes to set :silent = :no-op
"
  ([ind error-function steps print-progress-interval]
    (auto-simplify-plush ind error-function steps print-progress-interval
                         {{:silence 1} 0.5
                          {:silence 2} 0.3
                          {:silence 3} 0.1
                          {:silence 4} 0.1
                          ;{:silence 1 :unsilence 1} 0.05 ;Not used by default
                          ;{:silence 2 :unsilence 1} 0.1 ;Not used by default
                          ;{:silence 3 :unsilence 1} 0.05 ;Not used by default
                          ;{:no-op 1} 0.1 ;Not used by default
                          }))
  ([ind error-function steps print-progress-interval simplification-step-probabilities]
    (when (not (zero? print-progress-interval))
      (printf "\nAuto-simplifying Plush genome with starting size: %s" (count-points (:program ind))))
    (loop [step 0
           genome (:genome ind)
           program (if (:program ind)
                     (:program ind)
                     (translate-plush-genome-to-push-program ind
                                                             {:max-points (* 10 (count genome))}))
           errors (if (:errors ind)
                    (:errors ind)
                    (error-function program))]
;           total-errors (if (:total-error ind)
;                          (:total-error ind)
;                          (apply + errors))]
      (when (and (not (zero? print-progress-interval))
                 (or (>= step steps)
                     (zero? (mod step print-progress-interval))))
        (println "\nstep:" step)
        (println "genome:" (pr-str (not-lazy genome)))
        (println "program:" (pr-str (not-lazy program)))
        (println "errors:" (not-lazy errors))
        (println "genome size:" (count genome))
        (println "program size:" (count-points program)))
      (if (>= step steps)
        (make-individual :genome genome :program program :errors errors :total-error (apply + errors)
                         :history (:history ind) :genetic-operators :simplification)
        (let [new-genome (apply-simplification-step-to-genome genome simplification-step-probabilities)
              new-program (translate-plush-genome-to-push-program ind
                                                             {:max-points (* 10 (count genome))})
              new-errors (error-function new-program)]
          (if (= new-errors errors)
            (recur (inc step) new-genome new-program new-errors)
            (recur (inc step) genome program errors)))))))

;;;;;;;;;;;;;;;
(def ind-test
  (make-individual :genome 
                   '({:close 0, :instruction exec_shove}
                      {:close 0, :instruction exec_eq :silent true}
                      {:close 1, :instruction boolean_dup :silent false}
                      {:close 0, :instruction boolean_eq :silent true}
                      {:close 0,
                       :instruction code_stackdepth :silent false}
                      {:close 0,
                       :instruction boolean_yankdup}
                      {:close 0, :instruction 64}
                      {:close 0, :instruction code_flush}
                      {:close 1, :instruction code_yank}
                      {:close 1, :instruction exec_yank}
                      {:close 0, :instruction exec_swap}
                      {:close 1, :instruction code_shove}
                      {:close 0, :instruction integer_yank}
                      {:close 0, :instruction exec_rot}
                      {:close 0, :instruction code_flush}
                      {:close 0, :instruction integer_swap}
                      {:close 0, :instruction code_pop}
                      {:close 0, :instruction code_pop}
                      {:close 1, :instruction in1}
                      {:close 1, :instruction code_yank})))
;                   (random-plush-genome-with-size 20
;                                                  (concat (registered-for-stacks [:integer :boolean :code :exec])
;                                                          (list (fn [] (lrand-int 100))
;                                                                'in1))
;                                                  {:epigenetic-markers [:close]})))

(require 'clojush.problems.demos.odd)

(defn test-autosimp-genome
  [ind]
  (auto-simplify-plush ind
                       (:error-function clojush.problems.demos.odd/argmap)
                       5 ;1000
                       100))

(test-autosimp-genome ind-test)
