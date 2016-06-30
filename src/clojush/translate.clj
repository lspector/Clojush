(ns clojush.translate
  (:use [clojush util]
        clojush.instructions.common))

(defn delete-prev-paren-pair
  "Deletes the last closed paren pair from prog, which may be a partial program."
  [prog]
  (loop [reversed-prog (reverse prog)
         new-prog []
         number-close-parens 0
         found-first-close false]
    (cond
      ; Check if reversed-prog is empty, in which case we are done
      (empty? reversed-prog) (vec (reverse new-prog))
      ; Check if done, which is if we've found the first :close, the paren-stack is empty, and the first item in reversed-prog is :open
      (and found-first-close
           (zero? number-close-parens)
           (= :open (first reversed-prog))) (vec (reverse (concat new-prog (rest reversed-prog))))
      ; Check if looking for the correct :open but found an :open for a different paren
      (and found-first-close
           (< 0 number-close-parens)
           (= :open (first reversed-prog))) (recur (rest reversed-prog)
                                                   (conj new-prog (first reversed-prog))
                                                   (dec number-close-parens)
                                                   found-first-close)
      ; Check if looking for correct :open but found another :close
      (and found-first-close
           (= :close (first reversed-prog))) (recur (rest reversed-prog)
                                                    (conj new-prog (first reversed-prog))
                                                    (inc number-close-parens)
                                                    found-first-close)
      ; Check if just found first :close. In which case skip it and set the found-first-close flag
      (and (not found-first-close)
           (= :close (first reversed-prog))) (recur (rest reversed-prog)
                                                    new-prog
                                                    0
                                                    true)
      ; Otherwise, just put the item onto new-prog and keep looking with same other variables
      :else (recur (rest reversed-prog)
                   (conj new-prog (first reversed-prog))
                   number-close-parens
                   found-first-close))))

(defn translate-plush-genome-to-push-program
  "Takes as input an individual (or map) containing a Plush genome (:genome)
   and translates it to the correct Push program with
   balanced parens. The linear Plush genome is made up of a list of instruction
   maps, each including an :instruction key as well as other epigenetic marker
   keys. As the linear Plush genome is traversed, each instruction that requires
   parens will push :close and/or :close-open onto the paren-stack, and will
   also put an open paren after it in the program. For example, an instruction
   that requires 3 paren groupings will push :close, then :close-open, then :close-open.
   When a positive number is encountered in the :close key of the
   instruction map, it is set to num-parens-here during the next recur. This
   indicates the number of parens to put here, if need is indicated on the
   paren-stack. If the top item of the paren-stack is :close, a close paren
   will be inserted. If the top item is :close-open, a close paren followed by
   an open paren will be inserted.
   If the end of the program is reached but parens are still needed (as indicated by
   the paren-stack), parens are added until the paren-stack is empty.
   Instruction maps that have :silence set to true will be ignored entirely."
  [{:keys [genome program]}
   {:keys [max-points] :as argmap}]
  (if program
    program
    (let [translated-program
          (loop [prog [] ; The Push program incrementally being built
                 gn genome ; The linear Plush genome, where items will be popped off the front. Each item is a map containing at least the key :instruction, and unless the program is flat, also :close
                 num-parens-here 0 ; The number of parens that still need to be added at this location.
                 paren-stack '()] ; Whenever an instruction requires parens grouping, it will push either :close or :close-open on this stack. This will indicate what to insert in the program the next time a paren is indicated by the :close key in the instruction map.
            (cond
              ; Check if need to add close parens here
              (< 0 num-parens-here) (recur (cond
                                             (= (first paren-stack) :close) (conj prog :close)
                                             (= (first paren-stack) :close-open) (conj (conj prog :close) :open)
                                             :else prog) ; If paren-stack is empty, we won't put any parens in even though the :close epigenetic marker indicated to do so
                                           gn
                                           (dec num-parens-here)
                                           (rest paren-stack))
              ; Check if at end of program but still need to add parens
              (and (empty? gn)
                   (not (empty? paren-stack))) (recur prog
                                                      gn
                                                      (count paren-stack)
                                                      paren-stack)
              ; Check if done
              (empty? gn) (open-close-sequence-to-list (apply list prog))
              ; Check for silenced instruction
              (get (first gn) :silent false) (recur prog
                                                    (rest gn)
                                                    num-parens-here
                                                    paren-stack)
              ; If here, ready for next instruction
              :else (let [number-paren-groups (lookup-instruction-paren-groups (:instruction (first gn)))
                          new-paren-stack (if (>= 0 number-paren-groups)
                                            paren-stack
                                            (concat (repeat (dec number-paren-groups) :close-open)
                                                    '(:close)
                                                    paren-stack))]
                      (if (= 'noop_delete_prev_paren_pair (:instruction (first gn)))
                        (recur (delete-prev-paren-pair prog)
                               (rest gn)
                               (get (first gn) :close 0)
                               new-paren-stack)
                        (recur (if (= 'noop_open_paren (:instruction (first gn)))
                                 (conj prog :open)
                                 (if (>= 0 number-paren-groups)
                                   (conj prog (:instruction (first gn)))
                                   (conj (conj prog (:instruction (first gn))) :open)))
                               (rest gn)
                               (get (first gn) :close 0) ; The number of close parens to put after this instruction; if :close isn't in instruction map, default to zero
                               new-paren-stack)))))]
      (if (> (count-points translated-program) max-points)
        '() ; Translates to an empty programs if program exceeds max-points
        translated-program))))

(defn population-translate-plush-to-push
  "Converts the population of Plush genomes into Push programs."
  [pop-agents {:keys [use-single-thread] :as argmap}]
  (dorun (map #((if use-single-thread swap! send)
                    %
                    (fn [i] (assoc i :program (translate-plush-genome-to-push-program i argmap))))
              pop-agents))
  (when-not use-single-thread (apply await pop-agents))) ;; SYNCHRONIZE
