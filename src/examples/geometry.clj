;; geometry.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2012

(ns examples.geometry
  (:use [clojush]))

;;;;;;;;;;;;
;; Multiple geometric formulae. Boolean mode queries indicate the desired formula and
;; inputs are provided on the float stack.

;; Using just circle-related ones for now:
;
;0: circumference of a circle: 2 pi R
;1: area of a circle: pi R^2
;2: volume of a cylinder: pi R^2 H
;3: volume of a cone: 1/3 pi R^2 H
;4: volume of a sphere: 4/3 pi R^3

;== Some other possibilities for future inclusion: 
;perimeter of a square: 4 L
;area of a square: L^2
;perimeter of a rectangle: 2 (L + W)
;area of a rectangle: L W
;
;parallelogram...
;rhombus...
;triangle...
;
;volume of a rectangular prism: L W H
;volume of a triangular prism: 
;volume of a pyramid: 1/3 B H

(def fitness-cases
  (concat
    ;circumference of a circle: 2 pi R
    (doall (for [r (range 1 5 0.25)]
             [:circle-circumference [r 0] (* 2 Math/PI r)]))
    ;1: area of a circle: pi R^2
    (doall (for [r (range 1 5 0.25)]
             [:circle-area [r 0] (* Math/PI r r)]))
    ;2: volume of a cylinder: pi R^2 H
    (doall (for [r (range 1 2 0.25)
                 h (range 1 2 0.25)]
             [:cylinder-volume [r h] (* Math/PI r r h)]))
    ;3: volume of a cone: 1/3 pi R^2 H
    (doall (for [r (range 1 2 0.25)
                 h (range 1 2 0.25)]
             [:cone-volume [r h] (* 1/3 Math/PI r r h)]))
    ;4: volume of a sphere: 4/3 pi R^3
    (doall (for [r (range 1 5 0.25)]
             [:sphere-volume [r 0] (* 4/3 Math/PI r r r)]))
    ))

;; entry points

(def entry-point
  {:circle-circumference 'tagged_000
   :circle-area          'tagged_100
   :cylinder-volume      'tagged_200
   :cone-volume          'tagged_300
   :sphere-volume        'tagged_400})
 
;; input instructions

(define-registered r 
                   (fn [state] (push-item (first (stack-ref :auxiliary 0 state)) :float state)))

(define-registered h
                   (fn [state] (push-item (second (stack-ref :auxiliary 0 state)) :float state)))

(define-registered f1
                   (fn [state] (push-item '(r 3.141592 float_mult 2.0 float_mult) :exec state)))

(define-registered f2
                   (fn [state] (push-item '(r r float_mult 3.141592 float_mult) :exec state)))

(define-registered f3
                   (fn [state] (push-item '(r r float_mult h float_mult 3.141592 float_mult) :exec state)))

(define-registered f4
                   (fn [state] (push-item '(r r float_mult h float_mult 3.141592 float_mult 3.0 float_div) :exec state)))

(define-registered f5
                   (fn [state] (push-item '(r r float_mult r float_mult 3.141592 float_mult 4.0 float_mult 3.0 float_div) :exec state)))


;; sterile mimics
;;;;;;;;;;;;;;;;;;;;
(in-ns 'clojush)

(defn changed-over ;;SterileMimics
  [gens population population-size max-points atom-generators error-function]
  (let [filtered (filter #(or (< (count (:history %)) gens)
                              (not (= (first (:history %)) (nth (:history %) (dec gens)))))
                         population)]
    (if (empty? filtered)
      (do (println "Regenerating")
          (doall (map #(evaluate-individual % error-function (java.util.Random. (rand-int 1000000)))
                      (make-individual 
                        :program (random-code max-points atom-generators)))))
      (do (println "Number changed:" (count filtered))
          filtered))))


#_(defn pushgp
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
                      (let [pop (changed-over ;;SterileMimics
                                  2 ;;SterileMimics
                                  (decimate (vec (doall (map deref pop-agents))) ;;SterileMimics
                                          (int (* decimation-ratio population-size)) ;;SterileMimics
                                          decimation-tournament-size ;;SterileMimics
                                          trivial-geography-radius) ;;SterileMimics
                                  population-size max-points atom-generators error-function)] ;;SterileMimics
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



#_(defn mutate 
  "Returns a mutated version of the given individual."
  [ind mutation-max-points max-points atom-generators]
  (let [new-program (nth (iterate #(insert-code-at-point 
                                     % 
                                     (select-node-index %)
                                     (random-code mutation-max-points atom-generators))
                                  (:program ind))
                         (inc (lrand-int (inc (int (/ (count-points (:program ind)) 20))))))]
    (if (> (count-points new-program) max-points)
      (make-individual :program (random-code max-points atom-generators)
                       :history (:history ind)
                       :ancestors (if maintain-ancestors
                                    (cons (:program ind) (:ancestors ind))
                                    (:ancestors ind)))
      (make-individual :program new-program :history (:history ind)
        :ancestors (if maintain-ancestors
                     (cons (:program ind) (:ancestors ind))
                     (:ancestors ind))))))

#_(defn mutate 
  "Returns a mutated version of the given individual."
  [ind mutation-max-points max-points atom-generators]
  (let [new-program (insert-code-at-point (:program ind) 
                      (select-node-index (:program ind))
                      (random-code mutation-max-points atom-generators))]
    (if (> (count-points new-program) max-points)
      (make-individual :program (random-code max-points atom-generators)
                       :history (:history ind)
                       :ancestors (if maintain-ancestors
                                    (cons (:program ind) (:ancestors ind))
                                    (:ancestors ind)))
      (make-individual :program new-program :history (:history ind)
        :ancestors (if maintain-ancestors
                     (cons (:program ind) (:ancestors ind))
                     (:ancestors ind))))))

#_(defn crossover 
  "Returns a copy of parent1 with a random subprogram replaced with a random 
subprogram of parent2."
  [parent1 parent2 max-points]
  (let [new-program (nth (iterate #(insert-code-at-point
                                     %
                                     (select-node-index %)
                                     (code-at-point 
                                       (:program parent2)
                                       (select-node-index (:program parent2))))
                                  (:program parent1))
                         (inc (lrand-int (inc (int (/ (count-points (:program parent1)) 20))))))]
    (if (> (count-points new-program) max-points)
      (make-individual :program (random-code max-points @global-atom-generators) :history (:history parent1)
        :ancestors (if maintain-ancestors
                     (cons (:program parent1) (:ancestors parent1))
                     (:ancestors parent1)))
      (make-individual :program new-program :history (:history parent1)
        :ancestors (if maintain-ancestors
                     (cons (:program parent1) (:ancestors parent1))
                     (:ancestors parent1))))))


#_(defn crossover 
  "Returns a copy of parent1 with a random subprogram replaced with a random 
subprogram of parent2."
  [parent1 parent2 max-points]
  (let [new-program (insert-code-at-point 
                      (:program parent1) 
                      (select-node-index (:program parent1))
                      (code-at-point (:program parent2)
                        (select-node-index (:program parent2))))]
    (if (> (count-points new-program) max-points)
      (make-individual :program (random-code max-points @global-atom-generators) :history (:history parent1)
        :ancestors (if maintain-ancestors
                     (cons (:program parent1) (:ancestors parent1))
                     (:ancestors parent1)))
      (make-individual :program new-program :history (:history parent1)
        :ancestors (if maintain-ancestors
                     (cons (:program parent1) (:ancestors parent1))
                     (:ancestors parent1))))))



#_(postwalklist (fn [item] (if (and (seq? item) 
                                  (< (lrand) 0.5))
                           (into () (shuffle item)) ;; may reverse but we don't care
                           item))
              '(the (rain in) (spain (falls (mainly on the) plain))))

(defn mutate-nodal 
  [ind per-node-mutation-probability atom-generators]
  (make-individual 
    :program (postwalklist (fn [item] 
                             (if (< (lrand) per-node-mutation-probability)
                               (if (seq? item) 
                                 (into () (shuffle item)) ;; may reverse but we don't care
                                 (random-code 1 atom-generators))
                               item))
                           (:program ind))
    :history (:history ind)
    :ancestors (if maintain-ancestors
                 (cons (:program ind) (:ancestors ind))
                 (:ancestors ind))))



#_(defn mutate 
  "Returns a mutated version of the given individual."
  [ind mutation-max-points max-points atom-generators]
  (mutate-nodal ind 1/50 atom-generators))

#_(mutate (make-individual :program '(a b (c d) (e (f (g) h) (i j k))))
        100
        100
        [1 2 3])

(defn dominates
  [i1 i2]
  (let [error-pairs (map vector (:errors i1) (:errors i2))]
    (and (every? (fn [[e1 e2]] (<= e1 e2)) error-pairs)
         (some (fn [[e1 e2]] (< e1 e2)) error-pairs))))

(defn nondominated 
  [ind inds]
  (not (some #(dominates % ind) inds)))

#_(defn select ;; RETURN RANDOM CHOICE AMONG NONDOMINATED
  "Conducts a tournament and returns the individual with the lower total error."
  [pop tournament-size radius location]
  (let [tournament-set 
        (doall
          (for [_ (range tournament-size)]
            (nth pop
                 (if (zero? radius)
                   (lrand-int (count pop))
                   (mod (+ location (- (lrand-int (+ 1 (* radius 2))) radius))
                        (count pop))))))
        err-fn (if @global-use-historically-assessed-hardness :hah-error :total-error)]
    (lrand-nth (filter #(nondominated % tournament-set) tournament-set))))

(defn distinct-by-errors
  [population]
  (loop [result []
         errors #{}
         remaining population]
    (if (empty? remaining)
      result
      (if (some #{(:errors (first remaining))} errors)
        (recur result errors (rest remaining))
        (recur (conj result (first remaining))
               (conj errors (:errors (first remaining)))
               (rest remaining))))))

(defn smallest-distinct-by-errors
  [population]
  (vec (for [group (vals (group-by :errors population))]
         (reduce #(if (< (count-points (:program %1))
                         (count-points (:program %2)))
                    %1
                    %2)
                 group))))

(defn decimate
  [population target-size tournament-size radius]
  (let [survivors (smallest-distinct-by-errors (filter #(nondominated % population) population))]
    (println " Survivors:" (count survivors))
    survivors))
  
(in-ns 'examples.geometry)
;;;;;;;;;;;;;;;;;;;;


;; error function

(def e (fn [program]
         (doall 
           (for [[problem inputs target] fitness-cases]
             (let [pre-entry-state (run-push program (push-item inputs :auxiliary (make-push-state)))
                   post-entry-state (run-push (entry-point problem) pre-entry-state)
                   top-float (top-item :float post-entry-state)]
               (if (number? top-float)
                 (Math/abs (- top-float target))
                 1000000))))))
     
;; a solution, for testing purposes
#_(reduce + (e '(tag_exec_000 (r 3.141592 float_mult 2.0 float_mult)
               tag_exec_100 (r r float_mult 3.141592 float_mult)
               tag_exec_200 (r r float_mult h float_mult 3.141592 float_mult)
               tag_exec_300 (r r float_mult h float_mult 3.141592 float_mult 3.0 float_div)
               tag_exec_400 (r r float_mult r float_mult 3.141592 float_mult 4.0 float_mult 3.0 float_div))))

(pushgp 
  :atom-generators (concat (list ;(fn [] (lrand-int 5))
                                 (fn [] (float (lrand-int 5)))
                                 (fn [] (* 5.0 (lrand)))
                                 (fn [] Math/PI)
                                 'r
                                 'h
                                 ;'f1 
                                 ;'f2 
                                 ;'f3 
                                 'f4 'f5
                                 (tag-instruction-erc [:exec #_:integer #_:float #_:boolean] 1000)
                                 (tagged-instruction-erc 1000)
                                 (untag-instruction-erc 1000)
                                 ;(tagged-code-instruction-erc 1000)
                                 ;(tagged-when-instruction-erc 1000)
                                 )
                           ;; fortify with tagging and conditional vitamins
                           #_(take 15 (cycle [(tag-instruction-erc [:exec #_:integer :float #_:boolean] 1000)
                                            (tagged-instruction-erc 1000)
                                            (untag-instruction-erc 1000)
                                            ;(tagged-code-instruction-erc 1000)
                                            ;(tagged-when-instruction-erc 1000)
                                            ;'exec_if
                                            ;'exec_when
                                            ]))
                           #_'(integer_add
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
                           '(;float_lt
                              float_rot
                              ;float_yank
                              ;float_sin
                              ;float_frominteger
                              ;float_cos
                              ;float_stackdepth
                              float_swap
                              float_div
                              ;float_rand
                              ;float_shove
                              float_sub
                              ;float_flush
                              ;float_yankdup
                              ;float_fromboolean
                              ;float_gt
                              float_add
                              ;float_tan
                              float_mult
                              ;float_max
                              float_pop
                              ;float_eq
                              ;float_min
                              float_dup
                              ;float_mod
                              )
                           #_'(boolean_swap
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
                           #_'(exec_y
                              ;exec_fromziprights
                              exec_pop
                              exec_eq
                              ;exec_stackdepth
                              exec_rot
                              ;exec_do*times
                              ;exec_do*count
                              exec_s
                              ;exec_do*range
                              ;exec_fromzipnode
                              exec_if
                              exec_when
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
                              exec_noop)
                           #_'(code_nthcdr
                             code_insert
                             code_fromfloat
                             ;code_stackdepth
                             code_noop
                             code_subst
                             code_overlap
                             ;code_yankdup
                             ;code_fromziprights
                             code_null
                             code_pop
                             code_swap
                             code_append
                             code_member
                             code_do*
                             code_dup
                             code_quote
                             ;code_shove
                             code_cons
                             code_container
                             code_if
                             code_extract
                             code_wrap
                             ;code_fromziproot
                             code_nth
                             code_discrepancy
                             code_size
                             code_length
                             code_cdr
                             code_map
                             ;code_rand
                             code_atom
                             code_contains
                             code_list
                             ;code_do*range
                             ;code_fromzipnode
                             code_eq
                             ;code_fromzipchildren
                             ;code_flush
                             code_fromboolean
                             ;code_yank
                             code_frominteger
                             ;code_do*count
                             code_car
                             code_position
                             ;code_fromziplefts
                             code_do
                             code_do*times
                             code_rot)
                           )
  :error-function e
  :use-single-thread false
  :population-size 1000
  :trivial-geography-radius 0 ;10
  :error-threshold 0.01
  :max-generations 1001
  :max-points 200 ;250
  :evalpush-limit 400 ;500
  :evalpush-time-limit 0
  :tournament-size 7
  :reuse-errors true
  :mutation-probability 0.4 ;0.5
  :mutation-max-points 20
  :crossover-probability 0.4 ;0.5
  :simplification-probability 0.1 ;0.0
  :gaussian-mutation-probability 0.0
  :gaussian-mutation-per-number-mutation-probability 0.5
  :gaussian-mutation-standard-deviation 0.1
  :report-simplifications 100
  :final-report-simplifications 1000
  :reproduction-simplifications 10
  :decimation-ratio 1
  :decimation-tournament-size 2
  :node-selection-method :size-tournament
  :node-selection-tournament-size 2
  :node-selection-leaf-probability 0.1
  :pop-when-tagging true
  :use-historically-assessed-hardness false
  )

