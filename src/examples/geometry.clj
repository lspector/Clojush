;; geometry.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2012

(ns examples.geometry
  (:use [clojush :exclude [#_mutate #_select pushgp]])
  (:require [clojure.contrib.string :as string]))

;;;;;;;;;;;;
;; Multiple geometric formulae. An integer indicates the desired formula and
;; any needed inputs are provided on the float stack.

;== Some possibilities for inclusion: 
;perimeter of a square: 4 L
;area of a square: L^2
;perimeter of a rectangle: 2 (L + W)
;area of a rectangle: L W
;circumference of a circle: 2 pi R
;area of a circle: pi R^2
;
;parallelogram...
;rhombus...
;triangle...
;
;volume of a rectangular prism: L W H
;volume of a triangular prism: 
;volume of a cylinder: pi R^2 H
;volume of a cone: 1/3 pi R^2 H
;volume of a sphere: 4/3 pi R^3
;volume of a pyramid: 1/3 B H
;
;Using just circle-related ones for now:
;
;0: circumference of a circle: 2 pi R
;1: area of a circle: pi R^2
;2: volume of a cylinder: pi R^2 H
;3: volume of a cone: 1/3 pi R^2 H
;4: volume of a sphere: 4/3 pi R^3


(def fitness-cases
  (concat
    ;0: circumference of a circle: 2 pi R
    (doall (for [r (range 1 5 0.25)]
             [0 [r 0] (* 2 Math/PI r)]))
    ;1: area of a circle: pi R^2
    (doall (for [r (range 1 5 0.25)]
             [1 [r 0] (* Math/PI r r)]))
    ;2: volume of a cylinder: pi R^2 H
    (doall (for [r (range 1 2 0.25)
                 h (range 1 2 0.25)]
             [2 [r h] (* Math/PI r r h)]))
    ;3: volume of a cone: 1/3 pi R^2 H
    (doall (for [r (range 1 2 0.25)
                 h (range 1 2 0.25)]
             [3 [r h] (* 1/3 Math/PI r r h)]))
    ;4: volume of a sphere: 4/3 pi R^3
    (doall (for [r (range 1 5 0.25)]
             [4 [r 0] (* 4/3 Math/PI r r r)]))))


;;; tagging mutation

#_(defn mutate 
  "Returns a mutated version of the given individual."
  [ind mutation-max-points max-points atom-generators]
  (let [new-program 
        (if (zero? (rand-int 2))
          (insert-code-at-point (:program ind) 
                                (select-node-index (:program ind))
                                (random-code mutation-max-points atom-generators))
          (let [index (select-node-index (:program ind))]
            (insert-code-at-point (:program ind)
                                  index
                                  (let [tag (lrand-int 1000)] ;; hardcoded limit
                                        (list (symbol (str "tag_exec_" tag))
                                              (code-at-point (:program ind) index)
                                              (symbol (str "tagged_" tag)))))))]
    (if (> (count-points new-program) max-points)
      ind
      (make-individual :program new-program :history (:history ind)
                       :ancestors (if maintain-ancestors
                                    (cons (:program ind) (:ancestors ind))
                                    (:ancestors ind))))))

;;; lexicographic parsimony pressure

#_(defn select
  "Conducts a tournament and returns the individual with the lower total error."
  [pop tournament-size radius location]
  (let [tournament-set 
        (doall
          (for [_ (range tournament-size)]
            (nth pop
              (if (zero? radius)
                (lrand-int (count pop))
                (mod (+ location (- (lrand-int (+ 1 (* radius 2))) radius))
                  (count pop))))))]
    (reduce (fn [i1 i2] (cond (< (:total-error i1) (:total-error i2)) i1 
                              (< (:total-error i2) (:total-error i1)) i2
                              (< (count-points (:program i1)) (count-points (:program i2))) i1
                              :else i2))
	     tournament-set)))

;;; improve or die (marked ;;IOD)

(defn improved-over ;;IOD
  [gens population population-size max-points atom-generators error-function]
  (let [filtered (filter #(or (< (count (:history %)) gens)
                              (< (first (:history %)) (nth (:history %) (dec gens))))
                         population)]
    (if (empty? filtered)
      (do (println "Regenerating")
          (doall (map #(evaluate-individual % error-function (java.util.Random. (rand-int 1000000)))
                      (make-individual 
                        :program (random-code max-points atom-generators)))))
      (do (println "Number improved:" (count filtered))
          filtered))))


(defn changed-over ;;IOD
  [gens population population-size max-points atom-generators error-function]
  (let [filtered (filter #(or (< (count (:history %)) gens)
                              (not (= (first (:history %)) (nth (:history %) (dec gens)))))
                         population)]
    (if (empty? filtered)
      (do (println "Regenerating")
          (doall (map #(evaluate-individual % error-function (java.util.Random. (rand-int 1000000)))
                      (make-individual 
                        :program (random-code max-points atom-generators)))))
      (do (println "Number improved:" (count filtered))
          filtered))))

(defn improved-or-significantly-changed-over ;;IOD
  [gens population population-size max-points atom-generators error-function]
  (let [filtered (filter #(or (< (count (:history %)) gens)
                              (< (first (:history %)) (nth (:history %) (dec gens)))
                              (> (nth (:history %) (dec gens)) (+ 30 (first (:history %)))))
                         population)]
    (if (empty? filtered)
      (do (println "Regenerating")
          (doall (map #(evaluate-individual % error-function (java.util.Random. (rand-int 1000000)))
                      (make-individual 
                        :program (random-code max-points atom-generators)))))
      (do (println "Number improved:" (count filtered))
          filtered))))
  
  
(defn pushgp
  "The top-level routine of pushgp."
  [& {:keys [error-function error-threshold population-size max-points atom-generators max-generations
             max-mutations mutation-probability mutation-max-points crossover-probability 
             simplification-probability tournament-size report-simplifications final-report-simplifications
             reproduction-simplifications trivial-geography-radius decimation-ratio decimation-tournament-size
             evalpush-limit evalpush-time-limit node-selection-method node-selection-leaf-probability
             node-selection-tournament-size pop-when-tagging gaussian-mutation-probability 
             gaussian-mutation-per-number-mutation-probability gaussian-mutation-standard-deviation
             reuse-errors problem-specific-report use-single-thread random-seed 
             use-historically-assessed-hardness]
      :or {error-function (fn [p] '(0)) ;; pgm -> list of errors (1 per case)
           error-threshold 0
           population-size 1000
           max-points 50
           atom-generators (concat @registered-instructions
                                   (list 
                                     (fn [] (lrand-int 100))
                                     (fn [] (lrand))))
           max-generations 1001
           mutation-probability 0.4
           mutation-max-points 20
           crossover-probability 0.4
           simplification-probability 0.1
           tournament-size 7
           report-simplifications 100
           final-report-simplifications 1000
           reproduction-simplifications 1
           trivial-geography-radius 0
           decimation-ratio 1
           decimation-tournament-size 2
           evalpush-limit 150
           evalpush-time-limit 0
           node-selection-method :unbiased
           node-selection-leaf-probability 0.1
           node-selection-tournament-size 2
           pop-when-tagging true
           gaussian-mutation-probability 0.0
           gaussian-mutation-per-number-mutation-probability 0.5
           gaussian-mutation-standard-deviation 0.1
           reuse-errors true
           problem-specific-report default-problem-specific-report
           use-single-thread false
           random-seed (System/nanoTime)   
           use-historically-assessed-hardness false        
           }}]
  (binding [thread-local-random-generator (java.util.Random. random-seed)]
    ;; set globals from parameters
    (reset! global-atom-generators atom-generators)
    (reset! global-max-points-in-program max-points)
    (reset! global-evalpush-limit evalpush-limit)
    (reset! global-evalpush-time-limit evalpush-time-limit)
    (reset! global-node-selection-method node-selection-method)
    (reset! global-node-selection-leaf-probability node-selection-leaf-probability)
    (reset! global-node-selection-tournament-size node-selection-tournament-size)
    (reset! global-pop-when-tagging pop-when-tagging)
    (reset! global-reuse-errors reuse-errors)
    (reset! global-use-historically-assessed-hardness use-historically-assessed-hardness)
    (printf "\nStarting PushGP run.\n\n") (flush)
    (printf "Clojush version = ")
    (try
      (let [version-number (string/drop 1 (string/chop
                                            (re-find #"\".*\""
                                                     (first (string/split-lines
                                                              (local-file/slurp* "project.clj"))))))]
        (if (empty? version-number)
          (throw Exception)
          (printf (str version-number "\n"))))
      (flush)
      (catch Exception e
             (printf "version number unavailable\n")
             (flush)))
    (try
      (let [git-hash (git-last-commit-hash)]
        (if (empty? git-hash)
          (throw Exception)
          (do
            ;; NOTES: - Last commit hash will only be correct if this code has
            ;;          been committed already.
            ;;        - GitHub link will only work if commit has been pushed
            ;;          to GitHub.
            (printf (str "Hash of last Git commit = " git-hash "\n"))
            (printf (str "GitHub link = https://github.com/lspector/Clojush/commit/"
                         git-hash
                         "\n"))
            (flush))))
      (catch Exception e
             (printf "Hash of last Git commit = unavailable\n")
             (printf "GitHub link = unavailable\n")
             (flush)))
    (print-params 
      (error-function error-threshold population-size max-points atom-generators max-generations 
                      mutation-probability mutation-max-points crossover-probability
                      simplification-probability gaussian-mutation-probability 
                      gaussian-mutation-per-number-mutation-probability gaussian-mutation-standard-deviation
                      tournament-size report-simplifications final-report-simplifications
                      trivial-geography-radius decimation-ratio decimation-tournament-size evalpush-limit
                      evalpush-time-limit node-selection-method node-selection-tournament-size
                      node-selection-leaf-probability pop-when-tagging reuse-errors
                      use-single-thread random-seed use-historically-assessed-hardness
                      ))
    (printf "\nGenerating initial population...\n") (flush)
    (let [pop-agents (vec (doall (for [_ (range population-size)] 
                                   ((if use-single-thread atom agent)
                                        (make-individual 
                                          :program (random-code max-points atom-generators))
                                        :error-handler (fn [agnt except] (println except))))))
          child-agents (vec (doall (for [_ (range population-size)]
                                     ((if use-single-thread atom agent)
                                          (make-individual)
                                          :error-handler (fn [agnt except] (println except))))))
          rand-gens (vec (doall (for [k (range population-size)]
                                  (java.util.Random. (+ random-seed (inc k))))))]
      (loop [generation 0]
        (printf "\n\n-----\nProcessing generation: %s\nComputing errors..." generation) (flush)
        (dorun (map #((if use-single-thread swap! send) % evaluate-individual error-function %2) pop-agents rand-gens))
        (when-not use-single-thread (apply await pop-agents)) ;; SYNCHRONIZE ; might this need a dorun?
        (printf "\nDone computing errors.") (flush)
        ;; calculate solution rates if necessary for historically-assessed hardness
        (when use-historically-assessed-hardness
          (reset! solution-rates
                  (let [error-seqs (map :errors (map deref pop-agents))
                        num-cases (count (first error-seqs))]
                    (doall (for [i (range num-cases)]
                             (/ (count (filter #(<= % error-threshold) (map #(nth % i) error-seqs)))
                                population-size)))))
          (printf "\nSolution rates: ")
          (println (doall (map float @solution-rates))))
        ;; report and check for success
        (let [best (report (vec (doall (map deref pop-agents))) generation error-function 
                           report-simplifications problem-specific-report)]
          (if (<= (:total-error best) error-threshold)
            (do (printf "\n\nSUCCESS at generation %s\nSuccessful program: %s\nErrors: %s\nTotal error: %s\nHistory: %s\nSize: %s\n\n"
                        generation (not-lazy (:program best)) (not-lazy (:errors best)) (:total-error best) 
                        (not-lazy (:history best)) (count-points (:program best)))
                (when print-ancestors-of-solution
                  (printf "\nAncestors of solution:\n")
                  (println (:ancestors best)))
                (auto-simplify best error-function final-report-simplifications true 500))
            (do (if (>= generation max-generations)
                  (printf "\nFAILURE\n")
                  (do (printf "\nProducing offspring...") (flush)
                      (let [pop (;;IOD
                                  changed-over ;;IOD
                                  2 ;;IOD
                                  (decimate (vec (doall (map deref pop-agents))) ;;IOD
                                            (int (* decimation-ratio population-size)) ;;IOD
                                            decimation-tournament-size ;;IOD
                                            trivial-geography-radius) ;;IOD
                                  population-size max-points atom-generators error-function)] ;;IOD
                        (dotimes [i population-size]
                          ((if use-single-thread swap! send)
                               (nth child-agents i) 
                               breed i (nth rand-gens i) pop error-function population-size max-points atom-generators 
                               mutation-probability mutation-max-points crossover-probability 
                               simplification-probability tournament-size reproduction-simplifications 
                               trivial-geography-radius gaussian-mutation-probability 
                               gaussian-mutation-per-number-mutation-probability gaussian-mutation-standard-deviation)))
                      (when-not use-single-thread (apply await child-agents)) ;; SYNCHRONIZE
                      (printf "\nInstalling next generation...") (flush)
                      (dotimes [i population-size]
                        ((if use-single-thread swap! send)
                             (nth pop-agents i) (fn [av] (deref (nth child-agents i)))))
                      (when-not use-single-thread (apply await pop-agents)) ;; SYNCHRONIZE
                      (recur (inc generation)))))))))))

    
;; input instructions

(define-registered formula
                   (fn [state] (push-item (stack-ref :auxiliary 0 state) :integer state)))

(define-registered r 
                   (fn [state] (push-item (first (stack-ref :auxiliary 1 state)) :float state)))

(define-registered h
                   (fn [state] (push-item (second (stack-ref :auxiliary 1 state)) :float state)))

;; error function

(def e (fn [program]
         (doall 
           (for [[formula-number inputs target] fitness-cases]
             (let [state (run-push program 
                                   (push-item formula-number 
                                              :auxiliary 
                                              (push-item inputs 
                                                         :auxiliary
                                                         (make-push-state))))
                   top-float (top-item :float state)]
               (if (number? top-float)
                 (Math/abs (- top-float target))
                 1000000))))))
     
;; a partial solution, for testing purposes
#_(reduce + (e '(formula 4 integer_eq exec_if 
                         (r r float_mult r float_mult 3.141592 float_mult 4.0 float_mult 3.0 float_div)
                         (formula 0 integer_eq exec_if
                                  (r 3.141592 float_mult 2.0 float_mult)
                                  (formula 1 integer_eq exec_if
                                           (r r float_mult 3.141592 float_mult)
                                           (r r float_mult h float_mult 3.141592 float_mult))))))


(pushgp 
  :atom-generators (concat (list (fn [] (lrand-int 5))
                                 (fn [] (float (lrand-int 5)))
                                 (fn [] Math/PI)
                                 'formula
                                 'r
                                 'h
                                 (tag-instruction-erc [:exec :integer :float :boolean] 1000)
                                 (tagged-instruction-erc 1000)
                                 (tag-instruction-erc [:exec :integer :float :boolean] 1000)
                                 (tagged-instruction-erc 1000)
                                 (tag-instruction-erc [:exec :integer :float :boolean] 1000)
                                 (tagged-instruction-erc 1000)
                                 (tag-instruction-erc [:exec :integer :float :boolean] 1000)
                                 (tagged-instruction-erc 1000)
                                 (tag-instruction-erc [:exec :integer :float :boolean] 1000)
                                 (tagged-instruction-erc 1000)
                                 (tag-instruction-erc [:exec :integer :float :boolean] 1000)
                                 (tagged-instruction-erc 1000)
                                 (tag-instruction-erc [:exec :integer :float :boolean] 1000)
                                 (tagged-instruction-erc 1000)
                                 (tag-instruction-erc [:exec :integer :float :boolean] 1000)
                                 (tagged-instruction-erc 1000)
                                 (tag-instruction-erc [:exec :integer :float :boolean] 1000)
                                 (tagged-instruction-erc 1000)
                                 (tag-instruction-erc [:exec :integer :float :boolean] 1000)
                                 (tagged-instruction-erc 1000))
                           '(integer_add
                              integer_eq
                              integer_swap
                              ;integer_yank
                              integer_dup
                              ;integer_yankdup
                              integer_lt
                              ;integer_flush
                              ;integer_shove
                              integer_mult
                              ;integer_stackdepth
                              integer_div
                              integer_gt
                              integer_max
                              integer_fromfloat
                              integer_fromboolean
                              integer_sub
                              integer_mod
                              integer_rot
                              integer_min
                              ;integer_rand
                              integer_pop)
                           '(float_lt
                              float_rot
                              ;float_yank
                              float_sin
                              float_frominteger
                              float_cos
                              ;float_stackdepth
                              float_swap
                              float_div
                              ;float_rand
                              ;float_shove
                              float_sub
                              ;float_flush
                              ;float_yankdup
                              float_fromboolean
                              float_gt
                              float_add
                              float_tan
                              float_mult
                              float_max
                              float_pop
                              float_eq
                              float_min
                              float_dup
                              float_mod)
                           '(boolean_swap
                              boolean_eq
                              ;boolean_yank
                              boolean_fromfloat
                              ;boolean_flush
                              boolean_rot
                              boolean_and
                              ;boolean_rand
                              ;boolean_shove
                              boolean_not
                              boolean_or
                              boolean_frominteger
                              ;boolean_stackdepth
                              ;boolean_yankdup
                              boolean_dup
                              boolean_pop)
                           '(exec_y
                              ;exec_fromziprights
                              exec_pop
                              exec_eq
                              ;exec_stackdepth
                              exec_rot
                              exec_do*times
                              exec_do*count
                              exec_s
                              exec_do*range
                              ;exec_fromzipnode
                              exec_if
                              ;exec_fromziplefts
                              ;exec_fromzipchildren
                              exec_k
                              ;exec_yank
                              ;exec_flush
                              ;exec_yankdup
                              ;exec_fromziproot
                              exec_swap
                              exec_dup
                              ;exec_shove
                              exec_noop))
  :error-function e
  :population-size 1000
  :use-single-thread false
  :error-threshold 0.00001
  :max-points 100
  :max-generations 1001
  :mutation-probability 0.5 ;0.4 ;0.6 ;0.4
  :mutation-max-points 20
  :crossover-probability 0.5 ;0.4 ;0.2 ;0.3 ;0.4
  :simplification-probability 0.0 ;0.2 ;0.0 ;0.1
  :tournament-size 7
  :report-simplifications 100
  :final-report-simplifications 1000
  :reproduction-simplifications 10
  :trivial-geography-radius 500
  :decimation-ratio 1
  :decimation-tournament-size 2
  :evalpush-limit 200
  :evalpush-time-limit 0
  :node-selection-method :size-tournament ;:unbiased
  :node-selection-leaf-probability 0.1
  :node-selection-tournament-size 2
  :pop-when-tagging true
  :gaussian-mutation-probability 0.0
  :gaussian-mutation-per-number-mutation-probability 0.5
  :gaussian-mutation-standard-deviation 0.1
  :reuse-errors true
  :use-historically-assessed-hardness false
  )

