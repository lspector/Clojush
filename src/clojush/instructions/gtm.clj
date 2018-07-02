(ns clojush.instructions.gtm  
  (:use [clojush pushstate globals random util])
  (:require [clojure.math.numeric-tower :as math]))

;; All tapes except 0 are read-only.

;; The head on tape 0 auto-advances to the right after writing.

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
    {:active 0
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
;; Instructions for moving GTM control from tape to tape. There is always an "active"
;; tape, to which some instructions refer implicitly. 

(define-registered ;; set active to tape0
  gtm_tape0
  ^{:stack-types [:gtm]}
  (fn [state]
    (if (:gtm state)
      (trace 'gtm_tape0
             (-> state
                 (assoc-in [:gtm :active] 0)))
      state)))

(define-registered ;; set active to tape1
  gtm_tape1
  ^{:stack-types [:gtm]}
  (fn [state]
    (if (:gtm state)
      (trace 'gtm_tape1
             (-> state
                 (assoc-in [:gtm :active] 1)))
      state)))

(define-registered ;; set active to tape2
  gtm_tape2
  ^{:stack-types [:gtm]}
  (fn [state]
    (if (:gtm state)
      (trace 'gtm_tape2
             (-> state
                 (assoc-in [:gtm :active] 2)))
      state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instructions for moving the read/write head on the primary tape.

(define-registered ;; move head on active tape to the left
  gtm_left
  ^{:stack-types [:gtm]}
  (fn [state]
    (if (:gtm state)
      (trace 'gtm_left
             (update-in state [:gtm :tapes (:active (:gtm state)) :position] dec))
      state)))

(define-registered ;; move head on active tape to the right
  gtm_right
  ^{:stack-types [:gtm]}
  (fn [state]
    (if (:gtm state)
      (trace 'gtm_right
             (update-in state [:gtm :tapes (:active (:gtm state)) :position] inc))
      state)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instructions for reading/writing to tapes.

(define-registered ;; copy from active to tape 0
  gtm_copy
  ^{:stack-types [:gtm]}
  (fn [state]
    (if (and (:gtm state)
             (not (zero? (:active (:gtm state)))))
      (let [destination-tape (get (:tapes (:gtm state)) 0)
            destination-position (:position destination-tape)
            source (:active (:gtm state))
            source-tape (get (:tapes (:gtm state)) source)
            source-position (:position source-tape)
            source-instr-map (get (:contents source-tape) source-position)]
        (if source-instr-map
          (trace 'gtm_copy
                 (-> state
                     (assoc-in [:gtm :tapes 0 :contents destination-position] 
                               source-instr-map)
                     (update-in [:gtm :tapes 0 :position] inc)))
          state))
      state)))

(define-registered ;; push boolean indicating whether active tape is blank at current position
  gtm_blank
  ^{:stack-types [:gtm :boolean]}
  (fn [state]
    (if (:gtm state)
      (let [active (:active (:gtm state))
            active-tape (get (:tapes (:gtm state)) active)]
        (trace 'gtm_blank
               (push-item (if (get (:contents active-tape) (:position active-tape))
                            false
                            true)
                          :boolean 
                          state)))
      state)))

(define-registered ;; make tape 0 blank at current position
  gtm_erase
  ^{:stack-types [:gtm]}
  (fn [state]
    (if (and (:gtm state)
             (not (zero? (:active (:gtm state)))))
      (let [write-tape (get (:tapes (:gtm state)) 0)
            write-position (:position write-tape)]
        (trace 'gtm_erase
               (-> state
                   (assoc-in [:gtm :tapes 0 :contents]
                             (dissoc (:contents write-tape) write-position))
                   (update-in [:gtm :tapes 0 :position] inc))))
      state)))

(define-registered ;; push active tape's instruction to code stack
  gtm_instruction
  ^{:stack-types [:gtm :code]}
  (fn [state]
    (if (:gtm state)
      (let [active-tape (get (:tapes (:gtm state)) (:active (:gtm state)))
            instr-map (get (:contents active-tape) (:position active-tape))]
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
      (let [write-tape (get (:tapes (:gtm state)) 0)
            write-position (:position write-tape)
            write-contents (:contents write-tape)
            write-instr-map (ensure-instruction-map 
                              (get write-contents write-position))
            new-instr (first (flatten (ensure-list (stack-ref :code 0 state))))]
        (trace 'gtm_set_instruction
               (-> (pop-item :code state)
                   (assoc-in [:gtm :tapes 0 :contents write-position]
                             (assoc write-instr-map :instruction new-instr))
                   (update-in [:gtm :tapes 0 :position] inc))))
      state)))

(define-registered ;; push active tape's silent marker to boolean stack
  gtm_silent
  ^{:stack-types [:gtm :boolean]}
  (fn [state]
    (if (:gtm state)
      (let [active (:active (:gtm state))
            active-tape (get (:tapes (:gtm state)) active)
            instr-map (get (:contents active-tape) (:position active-tape))]
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
      (let [write-tape (get (:tapes (:gtm state)) 0)
            write-position (:position write-tape)
            write-contents (:contents write-tape)
            write-instr-map (ensure-instruction-map 
                              (get write-contents write-position))
            new-silent (stack-ref :boolean 0 state)]
        (trace 'gtm_set_silent
               (-> (pop-item :boolean state)
                   (assoc-in [:gtm :tapes 0 :contents write-position]
                             (assoc write-instr-map :silent new-silent))
                   (update-in [:gtm :tapes 0 :position] inc))))
      state)))

(define-registered ;; push active tape's close marker to integer stack
  gtm_close
  ^{:stack-types [:gtm :integer]}
  (fn [state]
    (if (:gtm state)
      (let [active (:active (:gtm state))
            active-tape (get (:tapes (:gtm state)) active)
            instr-map (get (:contents active-tape) (:position active-tape))]
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
      (let [write-tape (get (:tapes (:gtm state)) 0)
            write-position (:position write-tape)
            write-contents (:contents write-tape)
            write-instr-map (ensure-instruction-map 
                              (get write-contents write-position))
            new-close (math/abs (stack-ref :integer 0 state))]
        (trace 'gtm_set_close
               (-> (pop-item :integer state)
                   (assoc-in [:gtm :tapes 0 :contents write-position]
                             (assoc write-instr-map :close new-close))
                   (update-in [:gtm :tapes 0 :position] inc))))
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
#_(do 
    (use 'clojush.interpreter)
    (use 'clojush.instructions.code)
    (use 'clojush.instructions.boolean)
    (use 'clojush.instructions.random-instructions))

;; uniform crossover
#_(let [g1 (random-plush-genome 20 [0])
        g2 (random-plush-genome 20 [1])
        pgm '(gtm_tape1              ;; initial source is tape1
               exec_y                ;; repeatedly
               (gtm_copy            ;;   copy a gene
                 gtm_tape1 gtm_right ;;   move all heads to right
                 gtm_tape2 gtm_right ;;
                 boolean_rand        ;;   make next source randomly tape1 or tape2
                 exec_if gtm_tape1 gtm_tape2))
        run-pgm #(run-push pgm %)]
    (println "g1:" g1)(newline)
    (println "g2:" g2)(newline)
    (println "result:"
             (-> (make-push-state)
                 (init-gtm 3)
                 (load-tape 1 g1)
                 (load-tape 2 g2)
                 (assoc :autoconstructing true)
                 (run-pgm)
                 (dump-tape 0))))

;; back and forth on one parent
#_(let [g [{:instruction 1}{:instruction 2}{:instruction 3}]
        pgm '(true                  ;; top boolean indicates if moving right
               gtm_tape1           ;;   source is tape1
               exec_y               ;; repeatedly
               ( gtm_copy           ;;   copy a gene
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
                 (load-tape 1 g)
                 (run-pgm)
                 (dump-tape 0))))

;; following test requires global-atom-generators to be set
#_(reset! global-atom-generators [1 2 3])

;; 50% uniform mutation
#_(let [g (vec (repeat 20 {:instruction 0}))
        pgm '(gtm_tape1                           ;; source is tape1
               exec_y                             ;; repeatedly
               (boolean_rand                      ;;   randomly either
                 exec_if                          ;;
                 (100 code_rand                   ;;     set instruction randomly
                      gtm_set_instruction)
                 gtm_copy                         ;;     or copy it
                 gtm_tape1 gtm_right))
        run-pgm #(run-push pgm %)]
    (println "g:" g)(newline)
    (println "result:"
             (-> (make-push-state)
                 (init-gtm 3)
                 (load-tape 1 g)
                 (run-pgm)
                 (dump-tape 0))))

