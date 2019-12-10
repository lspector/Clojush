(ns clojush.instructions.gtm  
  (:use [clojush pushstate globals random util])
  (:require [clojure.math.numeric-tower :as math]))


;; Single, three-track tape (0, 1, 2).

;; Single position, with possible delay for tracks 1 and 2.

;; Only read/write/dub to track 0.

;; Read/write by component or all components together; dub only all together.
 
;; Read no-ops over blanks, write creates an instruction-map (gene) under
;; the head if none existed previously, with default instruction exec_noop,
;; silent false, and close 0.

;; All GTM instructions are no-ops on Push states that lack a GTM. 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gtm (genetic turing machine) utilities

(defn init-gtm
  "Returns the provided Push state, but with a fresh GTM that has been initialized."
  [push-state]
  (assoc push-state 
    :gtm
    {:position 0
     :delay 0
     :tracks (vec (repeat 3 (sorted-map)))
     :trace []}))

(defn ensure-instruction-map
  "If instr-map is a hash map, returns it unchanged; otherwise returns a default 
  instruction map."
  [instr-map]
  (merge {:instruction 'exec_noop
          :close 0
          :silent false
          :random-insertion false
          :uuid (java.util.UUID/randomUUID)}
         (if (map? instr-map)
           instr-map
           {})))

(defn load-track
  "Returns the provided Push state but with the indicated track in its GTM initialized 
  to contain the provided genome."
  [push-state track-index genome]
  (assoc-in push-state
            [:gtm :tracks track-index]
            (into (sorted-map)
                  (zipmap (iterate inc 0)
                          (map ensure-instruction-map genome)))))

(defn dump-track
  "Returns the genome recorded on the specified track in the GTM of the provided Push state."
  [push-state track-index]
  (vec (filter identity (vals (get (:tracks (:gtm push-state)) track-index)))))

(defn trace
  "Returns push-state with trace-info added to the trace recorded in its GTM."
  [trace-info push-state]
  (update-in push-state [:gtm :trace] conj trace-info))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gtm (genetic turing machine) instructions


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instructions for moving the read/write head

(define-registered ;; move head to the left
  gtm_left
  ^{:stack-types [:gtm]}
  (fn [state]
    (if (:gtm state)
      (trace 'gtm_left
             (update-in state [:gtm :position] dec))
      state)))

(define-registered ;; move head to the right
  gtm_right
  ^{:stack-types [:gtm]}
  (fn [state]
    (if (:gtm state)
      (trace 'gtm_right
             (update-in state [:gtm :position] inc))
      state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instructions to change the delay

(define-registered ;; increment delay
  gtm_inc_delay
  ^{:stack-types [:gtm]}
  (fn [state]
    (if (:gtm state)
      (trace 'gtm_inc_delay
             (update-in state [:gtm :delay] inc))
      state)))

(define-registered ;; decrement delay
  gtm_dec_delay
  ^{:stack-types [:gtm]}
  (fn [state]
    (if (:gtm state)
      (trace 'gtm_dec_delay
             (update-in state [:gtm :delay] dec))
      state)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instructions for copying information between tracks

(define-registered ;; dub gene at current position from track 1 (with delay) to track 0
  gtm_dub1
  ^{:stack-types [:gtm]}
  (fn [state]
    (if (:gtm state)
      (trace 'gtm_dub1
             (let [pos (:position (:gtm state))
                   delay (:delay (:gtm state))]
               (-> state
                   (assoc-in [:gtm :tracks 0 pos]
                             (get-in state [:gtm :tracks 1 (- pos delay)]))
                   (update-in [:gtm :position] inc))))
      state)))

(define-registered ;; dub gene at current position from track 2 (with delay) to track 0
  gtm_dub2
  ^{:stack-types [:gtm]}
  (fn [state]
    (if (:gtm state)
      (trace 'gtm_dub2
             (let [pos (:position (:gtm state))
                   delay (:delay (:gtm state))]
               (-> state
                   (assoc-in [:gtm :tracks 0 pos]
                             (get-in state [:gtm :tracks 2 (- pos delay)]))
                   (update-in [:gtm :position] inc))))
      state)))

(define-registered ;; bounce track 0 to track 1
  gtm_bounce1
  ^{:stack-types [:gtm]}
  (fn [state]
    (if (:gtm state)
      (trace 'gtm_bounce1
             (assoc-in state
                       [:gtm :tracks 1]
                       (get-in state [:gtm :tracks 0])))
      state)))

(define-registered ;; bounce track 0 to track 2
  gtm_bounce2
  ^{:stack-types [:gtm]}
  (fn [state]
    (if (:gtm state)
      (trace 'gtm_bounce2
             (assoc-in state
                       [:gtm :tracks 2]
                       (get-in state [:gtm :tracks 0])))
      state)))
                   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instructions for reading/writing

(define-registered ;; push boolean indicating whether track 0 is blank at current position
  gtm_blank0
  ^{:stack-types [:gtm :boolean]}
  (fn [state]
    (if (:gtm state)
      (trace 'gtm_blank0
             (let [pos (:position (:gtm state))]
               (push-item (if (get-in state [:gtm :tracks 0 pos])
                            false
                            true)
                          :boolean 
                          state)))
      state)))

(define-registered ;; push boolean indicating whether track 1 is blank at delayed position
  gtm_blank1
  ^{:stack-types [:gtm :boolean]}
  (fn [state]
    (if (:gtm state)
      (trace 'gtm_blank1
             (let [pos (:position (:gtm state))
                   delay (:delay (:gtm state))]
               (push-item (if (get-in state [:gtm :tracks 1 (- pos delay)])
                            false
                            true)
                          :boolean 
                          state)))
      state)))

(define-registered ;; push boolean indicating whether track 2 is blank at delayed position
  gtm_blank2
  ^{:stack-types [:gtm :boolean]}
  (fn [state]
    (if (:gtm state)
      (trace 'gtm_blank2
             (let [pos (:position (:gtm state))
                   delay (:delay (:gtm state))]
               (push-item (if (get-in state [:gtm :tracks 2 (- pos delay)])
                            false
                            true)
                          :boolean 
                          state)))
      state)))

(define-registered ;; make blank at current position
  gtm_erase
  ^{:stack-types [:gtm]}
  (fn [state]
    (if (:gtm state)
      (trace 'gtm_erase
             (let [pos (:position (:gtm state))]
               (-> state
                   (assoc-in [:gtm :tracks 0]
                             (dissoc (get-in state [:gtm :tracks 0]) pos))
                   ;(update-in [:gtm :position] inc)
                   )))
      state)))

(define-registered ;; push all components to stacks
  gtm_read_all
  ^{:stack-types [:gtm :code :boolean :integer]}
  (fn [state]
    (if (:gtm state)
      (let [pos (:position (:gtm state))
            instr-map (get-in state [:gtm :tracks 0 pos])]
        (if instr-map
          (trace 'gtm_read_all
                 (->> state
                      (push-item (:instruction instr-map) :code)
                      (push-item (:silent instr-map) :boolean)
                      (push-item (:close instr-map) :integer)))
          state))
      state)))

(define-registered ;; set all components from stacks
  gtm_write_all
  ^{:stack-types [:gtm :code :boolean :integer]}
  (fn [state]
    (if (and (:gtm state)
             (not (empty? (:code state)))
             (not (empty? (ensure-list (stack-ref :code 0 state))))
             (not (empty? (:boolean state)))
             (not (empty? (:integer state))))
      (let [pos (:position (:gtm state))
            write-instr-map (ensure-instruction-map 
                              (get-in state [:gtm :tracks 0 pos]))
            new-instr (first (flatten (ensure-list (stack-ref :code 0 state))))
            new-silent (stack-ref :boolean 0 state)
            new-close (math/abs (stack-ref :integer 0 state))]
        (trace 'gtm_write_all
               (-> (->> state
                        (pop-item :code)
                        (pop-item :boolean)
                        (pop-item :integer))
                   (assoc-in [:gtm :tracks 0 pos]
                             (-> write-instr-map
                                 (assoc :instruction new-instr)
                                 (assoc :silent new-silent)
                                 (assoc :close new-close)))
                   (update-in [:gtm :position] inc)
                   )))
      state)))

(define-registered ;; push instruction to code stack
  gtm_read_instruction
  ^{:stack-types [:gtm :code]}
  (fn [state]
    (if (:gtm state)
      (let [pos (:position (:gtm state))
            instr-map (get-in state [:gtm :tracks 0 pos])]
        (if instr-map
          (trace 'gtm_read_instruction
                 (push-item (:instruction instr-map) :code state))
          state))
      state)))

(define-registered ;; set instruction to first of flattened top of code stack
  gtm_write_instruction
  ^{:stack-types [:gtm :code]}
  (fn [state]
    (if (and (:gtm state)
             (not (empty? (:code state)))
             (not (empty? (ensure-list (stack-ref :code 0 state)))))
      (let [pos (:position (:gtm state))
            write-instr-map (ensure-instruction-map 
                              (get-in state [:gtm :tracks 0 pos]))
            new-instr (first (flatten (ensure-list (stack-ref :code 0 state))))]
        (trace 'gtm_write_instruction
               (-> (pop-item :code state)
                   (assoc-in [:gtm :tracks 0 pos]
                             (assoc write-instr-map :instruction new-instr))
                   (update-in [:gtm :position] inc)
                   )))
      state)))

(define-registered ;; push silent marker to boolean stack
  gtm_read_silent
  ^{:stack-types [:gtm :boolean]}
  (fn [state]
    (if (:gtm state)
      (let [pos (:position (:gtm state))
            instr-map (get-in state [:gtm :tracks 0 pos])]
        (if instr-map
          (trace 'gtm_read_silent
                 (push-item (:silent instr-map) :boolean state))
          state))
      state)))

(define-registered ;; set silent marker to top of boolean stack
  gtm_write_silent
  ^{:stack-types [:gtm :boolean]}
  (fn [state]
    (if (and (:gtm state)
             (not (empty? (:boolean state))))
      (let [pos (:position (:gtm state))
            write-instr-map (ensure-instruction-map 
                              (get-in state [:gtm :tracks 0 pos]))
            new-silent (stack-ref :boolean 0 state)]
        (trace 'gtm_write_silent
               (-> (pop-item :boolean state)
                   (assoc-in [:gtm :tracks 0 pos]
                             (assoc write-instr-map :silent new-silent))
                   (update-in [:gtm :position] inc)
                   )))
      state)))

(define-registered ;; push close marker to integer stack
  gtm_read_close
  ^{:stack-types [:gtm :integer]}
  (fn [state]
    (if (:gtm state)
      (let [pos (:position (:gtm state))
            instr-map (get-in state [:gtm :tracks 0 pos])]
        (if instr-map
          (trace 'gtm_read_close
                 (push-item (:close instr-map) :integer state))
          state))
      state)))

(define-registered ;; set close marker to abs value of top of integer stack
  gtm_write_close
  ^{:stack-types [:gtm :integer]}
  (fn [state]
    (if (and (:gtm state)
             (not (empty? (:integer state))))
      (let [pos (:position (:gtm state))
            write-instr-map (ensure-instruction-map 
                              (get-in state [:gtm :tracks 0 pos]))
            new-close (let [c (stack-ref :integer 0 state)]
                        (if (< c 0) (- c) c))]
        (trace 'gtm_write_close
               (-> (pop-item :integer state)
                   (assoc-in [:gtm :tracks 0 pos]
                             (assoc write-instr-map :close new-close))
                   (update-in [:gtm :position] inc)
                   )))
      state)))

(define-registered 
  exec_k_when_autoconstructing
  ^{:stack-types [:exec :genome]
    :parentheses 2}
  (fn [state]
    (if (and (:autoconstructing state)
             (not (empty? (rest (:exec state)))))
      (push-item (first (:exec state))
                 :exec
                 (pop-item :exec (pop-item :exec state)))
      state)))

(define-registered 
  exec_s_when_autoconstructing
  ^{:stack-types [:exec :genome]
    :parentheses 3}
  (fn [state]
    (if (and (:autoconstructing state)
             (not (empty? (rest (rest (:exec state))))))
      (let [stk (:exec state)
            x (first stk)
            y (first (rest stk))
            z (first (rest (rest stk)))]
        (if (and (<= (count-points (list y z)) @global-max-points)
                 (<= (height-of-nested-list (list y z)) @global-max-nested-depth))
          (push-item x
                     :exec
                     (push-item z
                                :exec
                                (push-item (list y z)
                                           :exec
                                           (pop-item :exec 
                                                     (pop-item :exec 
                                                               (pop-item :exec state))))))
          state))
      state)))

(define-registered 
  exec_y_when_autoconstructing
  ^{:stack-types [:exec :genome]
    :parentheses 1}
  (fn [state]
    (if (and (:autoconstructing state)
             (not (empty? (:exec state))))
      (let [new-item (list 'exec_y (first (:exec state)))]
        (if (and (<= (count-points new-item) @global-max-points)
                 (<= (height-of-nested-list new-item) @global-max-nested-depth))
          (push-item (first (:exec state))
                     :exec
                     (push-item new-item
                                :exec
                                (pop-item :exec state)))
          state))
      state)))

;; some tests

;; load and dump a random program
#_(let [genome (random-plush-genome 10 [1 'exec_noop 'integer_add]
                                  {:epigenetic-markers [:close :silent]})]
  (println "Before:" genome)
  (println "After loading and dumping:"
           (dump-track (load-track (init-gtm (make-push-state))
                                   1
                                   genome)
                       1)))

;; use namespaces for following tests
#_(do 
    (use 'clojush.interpreter)
    (use 'clojush.instructions.code)
    (use 'clojush.instructions.boolean)
    (use 'clojush.instructions.numbers)
    (use 'clojush.instructions.random-instructions)
    (use 'clojush.instructions.genome))


;; following tests require higher evalpush-limit to get to the end

#_(reset! global-evalpush-limit 10000)


;; uniform crossover
#_(let [g1 (repeat 20 {:instruction 0})
      g2 (repeat 20 {:instruction 1})
      pgm '(exec_y                
             (boolean_rand
               exec_if 
               gtm_dub1 
               gtm_dub2))
        run-pgm #(run-push pgm %)]
    (println "g1:" g1)(newline)
    (println "g2:" g2)(newline)
    (println "result:"
             (-> (make-push-state)
                 (init-gtm)
                 (load-track 1 g1)
                 (load-track 2 g2)
                 (assoc :autoconstructing true)
                 (run-pgm)
                 (dump-track 0))))

;; following test requires global-atom-generators to be set
#_(reset! global-atom-generators [1 2 3])

;; 50% uniform mutation
#_(let [g (vec (repeat 20 {:instruction 0}))
      pgm '(exec_y                
             (boolean_rand
               exec_if 
               (code_rand_atom
                 gtm_write_instruction
                 gtm_blank1 exec_when exec_flush)
               gtm_dub1))
        run-pgm #(run-push pgm %)]
    (println "g:" g)(newline)
    (println "result:"
             (-> (make-push-state)
                 (init-gtm)
                 (load-track 1 g)
                 (assoc :autoconstructing true)
                 (run-pgm)
                 (dump-track 0))))



;; alternation without alignment deviation
#_(let [g1 (mapv #(do {:instruction %}) (range 20))
      g2 (mapv #(do {:instruction %}) (map #(+ % 100) (range 20)))
      pgm '(true ;; true if taking from track 1
             exec_y  
             (exec_if
               (true gtm_dub1)
               (false gtm_dub2)
               integer_rand 3 integer_mod 0 integer_eq
               exec_when ;; alternate
               boolean_not
               ))
      run-pgm #(run-push pgm %)]
  (println "g1:" g1)(newline)
  (println "g2:" g2)(newline)
  (println "result:"
           (map :instruction (-> (make-push-state)
                                 (init-gtm)
                                 (load-track 1 g1)
                                 (load-track 2 g2)
                                 (run-pgm)
                                 (dump-track 0)))))


;; alternation with alignment deviation

#_(let [g1 (mapv #(do {:instruction %}) (range 20))
      g2 (mapv #(do {:instruction %}) (map #(+ % 100) (range 20)))
      pgm '(true ;; true if taking from track 1
             exec_y  
             (exec_if
               (true gtm_dub1)
               (false gtm_dub2)
               ;; possibly alternate
               integer_rand 3 integer_mod 0 integer_eq exec_when
               ;; alternating
               (boolean_not ;; switch track source
                 ;; possibly deviate
                 boolean_rand exec_when 
                 ;; deviating, randomly left or right
                 (boolean_rand exec_if gtm_inc_delay gtm_dec_delay))))
      run-pgm #(run-push pgm %)]
  (println "g1:" g1)(newline)
  (println "g2:" g2)(newline)
  (println "result:"
           (map :instruction (-> (make-push-state)
                                 (init-gtm)
                                 (load-track 1 g1)
                                 (load-track 2 g2)
                                 (run-pgm)
                                 (dump-track 0)))))

;; uniform mutation by addition and deletion (UMAD) with add rate 1/2 and delete rate 1/3 
#_(let [g (mapv #(do {:instruction %}) (map #(+ % 100) (range 20)))
      pgm '(exec_y ;; addition loop
             (boolean_rand exec_if ;; decide whether to add
               (code_rand_atom gtm_write_instruction gtm_inc_delay) ;; do addition
               gtm_dub1                                            ;; or just copy
               ;; end addition when hit a blank
               gtm_blank1 exec_when exec_pop)
             exec_y ;; deletion loop
             (integer_rand 3 integer_mod 0 integer_eq exec_when gtm_erase gtm_left))
      run-pgm #(run-push pgm %)]
  (println "g:" g)(newline)
  (println "result:"
           (map :instruction (-> (make-push-state)
                                 (init-gtm)
                                 (load-track 1 g)
                                 (run-pgm)
                                 (dump-track 0)))))


