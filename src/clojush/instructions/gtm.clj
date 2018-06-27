(ns clojush.instructions.gtm  
  (:use [clojush pushstate globals args random util])
  (:require [clojure.math.numeric-tower :as math]))

;; All GTM instructions are no-ops on Push states that lack a GTM.

;; Instructions that read from a tape are no-ops if the head is over a blank.

;; Instructions that write to a tape create an instruction-map (gene) under
;; the head if none existed previously, with default instruction exec_noop,
;; silent false, and close 0.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gtm (genetic turing machine) utilities

(defn init-gtm
  "Returns the provided Push state, but with a fresh GTM that has been initialized with  
  the given number of empty tapes."
  [push-state num-tapes]
  (assoc push-state 
    :gtm
    {:primary 0
     :secondary 0
     :tapes (vec (repeat num-tapes {:position 0 :contents (sorted-map)}))
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

(defn load-tape
  "Returns the provided Push state but with the indicated tape in its GTM initialized 
  to contain the provided genome."
  [push-state tape-index genome]
  (assoc-in push-state
            [:gtm :tapes tape-index :contents]
            (into (sorted-map)
                  (zipmap (iterate inc 0)
                          (map ensure-instruction-map genome)))))

(defn dump-tape
  "Returns the genome recorded on the specified tape in the GTM of the provided Push state."
  [push-state tape-index]
  (vec (vals (:contents (get (:tapes (:gtm push-state)) tape-index)))))

(defn trace
  "Returns push-state with trace-info added to the trace recorded in its GTM."
  [trace-info push-state]
  (update-in push-state [:gtm :trace] conj trace-info))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gtm (genetic turing machine) instructions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instructions for moving GTM control from tape to tape. There is always a "primary"
;; tape, to which most other instructions refer implicitly. When the primary tape is
;; changed, the prior primary is remembered as the "secondary" tape; the gtm_copy 
;; refers to both, copying an item from the secondary to the primary.

(define-registered ;; set primary to tape0, secondary to prior primary
  gtm_tape0
  ^{:stack-types [:gtm]}
  (fn [state]
    (if (:gtm state)
      (trace 'gtm_tape0
             (-> state
                 (assoc-in [:gtm :secondary] (:primary (:gtm state)))
                 (assoc-in [:gtm :primary] 0)))
      state)))

(define-registered ;; set primary to tape1, secondary to prior primary
  gtm_tape1
  ^{:stack-types [:gtm]}
  (fn [state]
    (if (:gtm state)
      (trace 'gtm_tape1
             (-> state
                 (assoc-in [:gtm :secondary] (:primary (:gtm state)))
                 (assoc-in [:gtm :primary] 1)))
      state)))

(define-registered ;; set primary to tape2, secondary to prior primary
  gtm_tape2
  ^{:stack-types [:gtm]}
  (fn [state]
    (if (:gtm state)
      (trace 'gtm_tape2
             (-> state
                 (assoc-in [:gtm :secondary] (:primary (:gtm state)))
                 (assoc-in [:gtm :primary] 2)))
      state)))

(define-registered ;; swap primary and secondary
  gtm_secondary
  ^{:stack-types [:gtm]}
  (fn [state]
    (if (:gtm state)
      (trace 'gtm_secondary
             (let [p (:primary (:gtm state))
                   s (:secondary (:gtm state))]
               (-> state
                   (assoc-in [:gtm :secondary] p)
                   (assoc-in [:gtm :primary] s))))
      state)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instructions for moving the read/write head on the primary tape.

(define-registered ;; move head on primary tape to the left
  gtm_left
  ^{:stack-types [:gtm]}
  (fn [state]
    (if (:gtm state)
      (trace 'gtm_left
             (update-in state [:gtm :tapes (:primary (:gtm state)) :position] dec))
      state)))

(define-registered ;; move head on primary tape to the right
  gtm_right
  ^{:stack-types [:gtm]}
  (fn [state]
    (if (:gtm state)
      (trace 'gtm_right
             (update-in state [:gtm :tapes (:primary (:gtm state)) :position] inc))
      state)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instructions for reading/writing to tapes.

(define-registered ;; copy from secondary to primary
  gtm_copy
  ^{:stack-types [:gtm]}
  (fn [state]
    (if (:gtm state)
      (let [primary (:primary (:gtm state))
            primary-tape (get (:tapes (:gtm state)) primary)
            primary-position (:position primary-tape)
            secondary (:secondary (:gtm state))
            secondary-tape (get (:tapes (:gtm state)) secondary)
            secondary-position (:position secondary-tape)
            secondary-instr-map (get (:contents secondary-tape) secondary-position)]
        (if secondary-instr-map
          (trace 'gtm_copy
                 (assoc-in state [:gtm :tapes primary :contents primary-position] 
                           secondary-instr-map))
          state))
      state)))

(define-registered ;; push boolean indicating whether primary tape is blank at current position
  gtm_blank
  ^{:stack-types [:gtm :boolean]}
  (fn [state]
    (if (:gtm state)
      (let [primary (:primary (:gtm state))
            primary-tape (get (:tapes (:gtm state)) primary)]
        (trace 'gtm_blank
               (push-item (if (get (:contents primary-tape) (:position primary-tape))
                            false
                            true)
                          :boolean 
                          state)))
      state)))

(define-registered ;; make primary tape is blank at current position
  gtm_erase
  ^{:stack-types [:gtm]}
  (fn [state]
    (if (:gtm state)
      (let [primary (:primary (:gtm state))
            primary-tape (get (:tapes (:gtm state)) primary)
            primary-position (:position primary-tape)]
        (trace 'gtm_erase
               (assoc-in state
                         [:gtm :tapes primary :contents]
                         (dissoc (:contents primary-tape) primary-position))))
      state)))

(define-registered ;; push primary tape's instruction to code stack
  gtm_instruction
  ^{:stack-types [:gtm :code]}
  (fn [state]
    (if (:gtm state)
      (let [primary-tape (get (:tapes (:gtm state)) (:primary (:gtm state)))
            instr-map (get (:contents primary-tape) (:position primary-tape))]
        (if instr-map
          (trace 'gtm_instruction
                 (push-item (:instruction instr-map) :code state))
          state))
      state)))

(define-registered ;; set instruction to first of flattened top of code stack
  gtm_set_instruction
  ^{:stack-types [:gtm :code]}
  (fn [state]
    (if (and (:gtm state)
             (not (empty? (:code state)))
             (not (empty? (ensure-list (stack-ref :code 0 state)))))
      (let [primary (:primary (:gtm state))
            primary-tape (get (:tapes (:gtm state)) primary)
            primary-position (:position primary-tape)
            primary-contents (:contents primary-tape)
            primary-instr-map (ensure-instruction-map 
                                (get primary-contents primary-position))
            new-instr (first (flatten (ensure-list (stack-ref :code 0 state))))]
        (trace 'gtm_set_instruction
               (assoc-in (pop-item :code state) 
                         [:gtm :tapes primary :contents primary-position]
                         (assoc primary-instr-map :instruction new-instr))))
      state)))

(define-registered ;; push primary tape's silent marker to boolean stack
  gtm_silent
  ^{:stack-types [:gtm :boolean]}
  (fn [state]
    (if (:gtm state)
      (let [primary (:primary (:gtm state))
            primary-tape (get (:tapes (:gtm state)) primary)
            instr-map (get (:contents primary-tape) (:position primary-tape))]
        (if instr-map
          (trace 'gtm_silent
                 (push-item (:silent instr-map) :boolean state))
          state))
      state)))

(define-registered ;; set silent to top of boolean stack
  gtm_set_silent
  ^{:stack-types [:gtm :boolean]}
  (fn [state]
    (if (and (:gtm state)
             (not (empty? (:boolean state))))
      (let [primary (:primary (:gtm state))
            primary-tape (get (:tapes (:gtm state)) primary)
            primary-position (:position primary-tape)
            primary-contents (:contents primary-tape)
            primary-instr-map (ensure-instruction-map 
                                (get primary-contents primary-position))
            new-silent (stack-ref :boolean 0 state)]
        (trace 'gtm_set_silent
               (assoc-in (pop-item :boolean state)
                         [:gtm :tapes primary :contents primary-position]
                         (assoc primary-instr-map :silent new-silent))))
      state)))

(define-registered ;; push primary tape's close marker to integer stack
  gtm_close
  ^{:stack-types [:gtm :integer]}
  (fn [state]
    (if (:gtm state)
      (let [primary (:primary (:gtm state))
            primary-tape (get (:tapes (:gtm state)) primary)
            instr-map (get (:contents primary-tape) (:position primary-tape))]
        (if instr-map
          (trace 'gtm_close
                 (push-item (:close instr-map) :integer state))
          state))
      state)))

(define-registered ;; set close to abs value of top of integer stack
  gtm_set_close
  ^{:stack-types [:gtm :integer]}
  (fn [state]
    (if (and (:gtm state)
             (not (empty? (:integer state))))
      (let [primary (:primary (:gtm state))
            primary-tape (get (:tapes (:gtm state)) primary)
            primary-position (:position primary-tape)
            primary-contents (:contents primary-tape)
            primary-instr-map (ensure-instruction-map 
                                (get primary-contents primary-position))
            new-close (math/abs (stack-ref :integer 0 state))]
        (trace 'gtm_set_close
               (assoc-in (pop-item :integer state)
                         [:gtm :tapes primary :contents primary-position]
                         (assoc primary-instr-map :close new-close))))
      state)))

;; some tests

;; load and dump a random program
#_(let [genome (random-plush-genome 10 [1 'exec_noop 'integer_add]
                                  {:epigenetic-markers [:close :silent]})]
  (println "Before:" genome)
  (println "After loading and dumping:"
           (dump-tape (load-tape (init-gtm (make-push-state) 3)
                                 1
                                 genome)
                      1)))

;; use namespaces for following tests
(do 
  (use 'clojush.interpreter)
  (use 'clojush.instructions.code)
  (use 'clojush.instructions.boolean)
  (use 'clojush.instructions.random-instructions))

;; uniform crossover
#_(let [g0 (random-plush-genome 20 [0])
      g1 (random-plush-genome 20 [1])
      pgm '(gtm_tape0              ;; initial source is tape0
             exec_y                ;; repeatedly
             (gtm_tape2            ;;   set destination to tape2
               gtm_copy            ;;   copy a gene
               gtm_tape0 gtm_right ;;   move all heads to right
               gtm_tape1 gtm_right ;;
               gtm_tape2 gtm_right ;;
               boolean_rand        ;;   make next source randomly tape0 or tape1
               exec_if gtm_tape0 gtm_tape1))
      run-pgm #(run-push pgm %)]
  (println "g0:" g0)(newline)
  (println "g1:" g1)(newline)
  (println "result:"
           (-> (make-push-state)
               (init-gtm 3)
               (load-tape 0 g0)
               (load-tape 1 g1)
               (run-pgm)
               (dump-tape 2))))

;; back and forth on one parent
#_(let [g [{:instruction 1}{:instruction 2}{:instruction 3}]
      pgm '(true                  ;; top boolean indicates if moving right
             exec_y               ;; repeatedly
             (gtm_tape0           ;;   source is tape0
               gtm_tape2          ;;   destination is tape2
               gtm_copy           ;;   copy a gene
               gtm_right          ;;   move destination to right
               gtm_tape0          ;;   advance source in current direction
               exec_if
               (true gtm_right)
               (false gtm_left)
               gtm_blank          ;;   if blank, reverse and move twice
               exec_if
               (exec_if false true
                        exec_if
                        (true gtm_right gtm_right)
                        (false gtm_left gtm_left))
               ()))
      run-pgm #(run-push pgm %)]
  (println "g:" g)(newline)
  (println "result:"
           (-> (make-push-state)
               (init-gtm 3)
               (load-tape 0 g)
               (run-pgm)
               (dump-tape 2))))

;; following test requires global-atom-generators to be set
#_(reset! global-atom-generators [1 2 3])

;; 50% uniform mutation
#_(let [g (vec (repeat 20 {:instruction 0}))
      pgm '(gtm_tape0                 ;; source is tape0
             gtm_tape2                ;; destination is tape2
             exec_y                   ;; repeatedly
             (boolean_rand            ;;   randomly either
               exec_if                ;;
               (100 code_rand         ;;     set instruction randomly
                 gtm_set_instruction)
               gtm_copy               ;;     or copy it
               gtm_tape0 gtm_right    ;;   in either case advance both tapes
               gtm_tape2 gtm_right))
      run-pgm #(run-push pgm %)]
  (println "g:" g)(newline)
  (println "result:"
           (-> (make-push-state)
               (init-gtm 3)
               (load-tape 0 g)
               (run-pgm)
               (dump-tape 2))))

