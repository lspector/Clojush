(ns clojush.instructions.genome  
  (:use [clojush pushstate globals]
        clojush.instructions.common))

(define-registered genome_pop (with-meta (popper :genome) {:stack-types [:genome]}))
(define-registered genome_dup (with-meta (duper :genome) {:stack-types [:genome]}))
(define-registered genome_swap (with-meta (swapper :genome) {:stack-types [:genome]}))
(define-registered genome_rot (with-meta (rotter :genome) {:stack-types [:genome]}))
(define-registered genome_flush (with-meta (flusher :genome) {:stack-types [:genome]}))
(define-registered genome_eq (with-meta (eqer :genome) {:stack-types [:genome :boolean]}))
(define-registered genome_stackdepth (with-meta (stackdepther :genome) {:stack-types [:genome :integer]}))
(define-registered genome_yank (with-meta (yanker :genome) {:stack-types [:genome :integer]}))
(define-registered genome_yankdup (with-meta (yankduper :genome) {:stack-types [:genome :integer]}))
(define-registered genome_shove (with-meta (shover :genome) {:stack-types [:genome :integer]}))
(define-registered genome_empty (with-meta (emptyer :genome) {:stack-types [:genome :boolean]}))

(define-registered
  genome_gene_dup
  ^{:stack-types [:genome :integer]}
  (fn [state]
    (if (and (not (empty? (:integer state)))
             (not (empty? (:genome state)))
             (not (empty? (stack-ref :genome 0 state))))
      (let [genome (stack-ref :genome 0 state)
            index (mod (stack-ref :integer 0 state) (count genome))]
        (->> (pop-item :integer state)
             (pop-item :genome)
             (push-item (concat (take (inc index) genome)
                                (drop index genome))
                        :genome)))
      state)))

(define-registered
  genome_gene_randomize
  ^{:stack-types [:genome :integer]}
  (fn [state]
    (if (and (not (empty? (:integer state)))
             (not (empty? (:genome state)))
             (not (empty? (stack-ref :genome 0 state))))
      (let [genome (stack-ref :genome 0 state)
            index (mod (stack-ref :integer 0 state) (count genome))]
        (->> (pop-item :integer state)
             (pop-item :genome)
             (push-item (concat (take index genome)
                                (list (rand-nth (:random-genome state)))
                                (drop (inc index) genome))
                        :genome)))
      state)))

(define-registered
  genome_gene_delete
  ^{:stack-types [:genome :integer]}
  (fn [state]
    (if (and (not (empty? (:integer state)))
             (not (empty? (:genome state)))
             (not (empty? (stack-ref :genome 0 state))))
      (let [genome (stack-ref :genome 0 state)
            index (mod (stack-ref :integer 0 state) (count genome))]
        (->> (pop-item :integer state)
             (pop-item :genome)
             (push-item (concat (take index genome)
                                (drop (inc index) genome))
                        :genome)))
      state)))

(define-registered
  genome_rotate
  ^{:stack-types [:genome :integer]}
  (fn [state]
    (if (and (not (empty? (:integer state)))
             (not (empty? (:genome state)))
             (not (empty? (stack-ref :genome 0 state))))
      (let [genome (stack-ref :genome 0 state)
            distance (mod (stack-ref :integer 0 state) (count genome))]
        (->> (pop-item :integer state)
             (pop-item :genome)
             (push-item (concat (drop distance genome)
                                (take distance genome))
                        :genome)))
      state)))

(define-registered
  genome_gene_copy
  ^{:stack-types [:genome :integer]}
  ;; copies from the second genome to the first
  ;; index is into source -- if destination is too short it will be added to end
  (fn [state]
    (if (and (not (empty? (:integer state)))
             (not (empty? (rest (:genome state))))
             (not (empty? (stack-ref :genome 1 state))))
      (let [source (stack-ref :genome 1 state)
            destination (stack-ref :genome 0 state)
            index (mod (stack-ref :integer 0 state) (count source))]
        (->> (pop-item :integer state)
             (pop-item :genome)
             (push-item (seq (assoc (vec destination)
                                    (min index (count destination))
                                    (nth source index)))
                        :genome)))
      state)))

(define-registered
  genome_toggle_silent
  ^{:stack-types [:genome :integer]}
  (fn [state]
    (if (and (not (empty? (:integer state)))
             (not (empty? (:genome state)))
             (not (empty? (stack-ref :genome 0 state))))
      (let [genome (stack-ref :genome 0 state)
            index (mod (stack-ref :integer 0 state) (count genome))]
        (->> (pop-item :integer state)
             (pop-item :genome)
             (push-item (concat (take index genome)
                                (let [g (nth genome index)]
                                  (list (assoc g :silent (not (:silent g)))))
                                (drop (inc index) genome))
                        :genome)))
      state)))

(define-registered
  genome_silence
  ^{:stack-types [:genome :integer]}
  (fn [state]
    (if (and (not (empty? (:integer state)))
             (not (empty? (:genome state)))
             (not (empty? (stack-ref :genome 0 state))))
      (let [genome (stack-ref :genome 0 state)
            index (mod (stack-ref :integer 0 state) (count genome))]
        (->> (pop-item :integer state)
             (pop-item :genome)
             (push-item (concat (take index genome)
                                (let [g (nth genome index)]
                                  (list (assoc g :silent true)))
                                (drop (inc index) genome))
                        :genome)))
      state)))

(define-registered
  genome_unsilence
  ^{:stack-types [:genome :integer]}
  (fn [state]
    (if (and (not (empty? (:integer state)))
             (not (empty? (:genome state)))
             (not (empty? (stack-ref :genome 0 state))))
      (let [genome (stack-ref :genome 0 state)
            index (mod (stack-ref :integer 0 state) (count genome))]
        (->> (pop-item :integer state)
             (pop-item :genome)
             (push-item (concat (take index genome)
                                (let [g (nth genome index)]
                                  (list (assoc g :silent false)))
                                (drop (inc index) genome))
                        :genome)))
      state)))

(define-registered
  genome_close_inc
  ^{:stack-types [:genome :integer]}
  (fn [state]
    (if (and (not (empty? (:integer state)))
             (not (empty? (:genome state)))
             (not (empty? (stack-ref :genome 0 state))))
      (let [genome (stack-ref :genome 0 state)
            index (mod (stack-ref :integer 0 state) (count genome))]
        (->> (pop-item :integer state)
             (pop-item :genome)
             (push-item (concat (take index genome)
                                (let [g (nth genome index)]
                                  (list (assoc g :close (inc (:close g)))))
                                (drop (inc index) genome))
                        :genome)))
      state)))

(define-registered
  genome_close_dec
  ^{:stack-types [:genome :integer]}
  (fn [state]
    (if (and (not (empty? (:integer state)))
             (not (empty? (:genome state)))
             (not (empty? (stack-ref :genome 0 state))))
      (let [genome (stack-ref :genome 0 state)
            index (mod (stack-ref :integer 0 state) (count genome))]
        (->> (pop-item :integer state)
             (pop-item :genome)
             (push-item (concat (take index genome)
                                (let [g (nth genome index)]
                                  (list (assoc g :close (max 0 (dec (:close g))))))
                                (drop (inc index) genome))
                        :genome)))
      state)))

(define-registered
  genome_new
  ^{:stack-types [:genome]}
  (fn [state]
    (push-item () :genome state)))

(define-registered
  genome_parent1
  ^{:stack-types [:genome]}
  (fn [state]
    (push-item (:parent1-genome state) :genome state)))

(define-registered
  genome_parent2
  ^{:stack-types [:genome]}
  (fn [state]
    (push-item (:parent2-genome state) :genome state)))

(define-registered
  genome_random
  ^{:stack-types [:genome]}
  (fn [state]
    (push-item (:random-genome state) :genome state)))

