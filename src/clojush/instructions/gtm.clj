(ns clojush.instructions.gtm  
  (:use [clojush pushstate globals random util])
  (:require [clojure.math.numeric-tower :as math]))


;; Single, two-track tape with single head position.

;; Only read/write to one track.

;; Second track accessed only via gene/component swaps at current location.

;; Read/write by component or all components together.

;; Read (but not swap) no-ops over blanks, write creates an instruction-map 
;; (gene) under the head if none existed previously, with default instruction
;; exec_noop, silent false, and close 0.

;; All GTM instructions are no-ops on Push states that lack a GTM. 

;; ....“insert” versions of all of the write instructions, which add whole cells, across both tracks. And gtm_delete along with the existing gtm_erase.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gtm (genetic turing machine) utilities

(defn init-gtm
  "Returns the provided Push state, but with a fresh GTM that has been initialized."
  [push-state]
  (assoc push-state 
    :gtm
    {:position 0
     :tracks (vec (repeat 2 (sorted-map)))
     :trace []
     :skew 0}))

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
;; Instructions for changing the skew

(define-registered ;; skew to the left
  gtm_skew_left
  ^{:stack-types [:gtm]}
  (fn [state]
    (if (:gtm state)
      (trace 'gtm_skew_left
             (update-in state [:gtm :skew] dec))
      state)))

(define-registered ;; skew to the right
  gtm_skew_right
  ^{:stack-types [:gtm]}
  (fn [state]
    (if (:gtm state)
      (trace 'gtm_skew_right
             (update-in state [:gtm :skew] inc))
      state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instructions for swapping genes between tracks

(define-registered ;; swap genes at current position between tracks 0 and 1
  gtm_swap_all
  ^{:stack-types [:gtm]}
  (fn [state]
    (if (:gtm state)
      (trace 'gtm_swap_all
             (let [pos (:position (:gtm state))
                   skew (:skew (:gtm state))]
               (-> state
                   (assoc-in [:gtm :tracks 0 pos]
                             (get-in state [:gtm :tracks 1 (+ pos skew)]))
                   (assoc-in [:gtm :tracks 1 (+ pos skew)]
                             (get-in state [:gtm :tracks 0 pos])))))
      state)))

(define-registered ;; swap instructions at current position between tracks 0 and 1
  gtm_swap_instruction
  ^{:stack-types [:gtm]}
  (fn [state]
    (if (:gtm state)
      (trace 'gtm_swap_instruction
             (let [pos (:position (:gtm state))
                   skew (:skew (:gtm state))]
               (-> state
                   (assoc-in [:gtm :tracks 0 pos :instruction]
                             (get-in state [:gtm :tracks 1 (+ pos skew) :instruction]))
                   (assoc-in [:gtm :tracks 1 (+ pos skew) :instruction]
                             (get-in state [:gtm :tracks 0 pos :instruction])))))
      state)))

(define-registered ;; swap silent markers at current position between tracks 0 and 1
  gtm_swap_silent
  ^{:stack-types [:gtm]}
  (fn [state]
    (if (:gtm state)
      (trace 'gtm_swap_silent
             (let [pos (:position (:gtm state))
                   skew (:skew (:gtm state))]
               (-> state
                   (assoc-in [:gtm :tracks 0 pos :silent]
                             (get-in state [:gtm :tracks 1 (+ pos skew) :silent]))
                   (assoc-in [:gtm :tracks 1 (+ pos skew) :silent]
                             (get-in state [:gtm :tracks 0 pos :silent])))))
      state)))

(define-registered ;; swap close markers at current position between tracks 0 and 1
  gtm_swap_close
  ^{:stack-types [:gtm]}
  (fn [state]
    (if (:gtm state)
      (trace 'gtm_swap_close
             (let [pos (:position (:gtm state))
                   skew (:skew (:gtm state))]
               (-> state
                   (assoc-in [:gtm :tracks 0 pos :close]
                             (get-in state [:gtm :tracks 1 (+ pos skew) :close]))
                   (assoc-in [:gtm :tracks 1 (+ pos skew) :close]
                             (get-in state [:gtm :tracks 0 pos :close])))))
      state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instructions for reading/writing to track 0

(define-registered ;; push boolean indicating whether blank at current position
  gtm_blank
  ^{:stack-types [:gtm :boolean]}
  (fn [state]
    (if (:gtm state)
      (trace 'gtm_blank
             (let [pos (:position (:gtm state))]
               (push-item (if (get-in state [:gtm :tracks 0 pos])
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
               (assoc-in state
                         [:gtm :tracks 0]
                         (dissoc (get-in state [:gtm :tracks 0]) pos))))
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
                 (push-item (:instruction instr-map) :code state)
                 (push-item (:silent instr-map) :boolean state)
                 (push-item (:close instr-map) :integer state))
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
               (-> (pop-item :code state)
                   (pop-item :boolean state)
                   (pop-item :integer state)
                   (assoc-in [:gtm :tracks 0 pos]
                             (-> write-instr-map
                                 (assoc :instruction new-instr)
                                 (assoc :silent new-silent)
                                 (assoc :close new-close))))))
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
                             (assoc write-instr-map :instruction new-instr)))))
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
                             (assoc write-instr-map :silent new-silent)))))
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
            new-close (math/abs (stack-ref :integer 0 state))]
        (trace 'gtm_write_close
               (-> (pop-item :integer state)
                   (assoc-in [:gtm :tracks 0 pos]
                             (assoc write-instr-map :close new-close)))))
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
(do 
    (use 'clojush.interpreter)
    (use 'clojush.instructions.code)
    (use 'clojush.instructions.boolean)
    (use 'clojush.instructions.numbers)
    (use 'clojush.instructions.random-instructions))

;; uniform crossover
#_(let [g1 (random-plush-genome 20 [0])
      g2 (random-plush-genome 20 [1])
      pgm '(exec_y                
             (boolean_rand
               exec_if 
               gtm_swap_all 
               exec_noop
               gtm_right))
        run-pgm #(run-push pgm %)]
    (println "g1:" g1)(newline)
    (println "g2:" g2)(newline)
    (println "result:"
             (-> (make-push-state)
                 (init-gtm)
                 (load-track 0 g1)
                 (load-track 1 g2)
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
               (100 code_rand
                    gtm_write_instruction)
               exec_noop
               gtm_right))
        run-pgm #(run-push pgm %)]
    (println "g:" g)(newline)
    (println "result:"
             (-> (make-push-state)
                 (init-gtm)
                 (load-track 0 g)
                 (run-pgm)
                 (dump-track 0))))


;; following test requires higher evalpush-limit to get to the end

(reset! global-evalpush-limit 1000)

;; alternation without alignment deviation
(let [g1 (mapv #(do {:instruction %}) (range 20))
      g2 (mapv #(do {:instruction %}) (map #(+ % 100) (range 20)))
      pgm '(true ;; true if taking from track 1
             exec_y  
             (exec_if
               (true gtm_swap_all)
               false
               gtm_right
               integer_rand 3 integer_mod 0 integer_eq
               exec_if ;; alternate
               boolean_not
               exec_noop
               ))
        run-pgm #(run-push pgm %)]
    (println "g1:" g1)(newline)
    (println "g2:" g2)(newline)
    (println "result:"
             (-> (make-push-state)
                 (init-gtm)
                 (load-track 0 g1)
                 (load-track 1 g2)
                 (run-pgm)
                 (dump-track 0))))


;; alternation with alignment deviation -- not yet


;; UMAD -- not yet

