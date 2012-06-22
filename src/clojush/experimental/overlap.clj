(ns clojush.experimental.overlap
  (:use [clojush.pushstate]
        [clojush.util])
  (:require [clojure.math.numeric-tower :as math]))

(defn discrepancy
  "Returns a measure of the discrepancy between list1 and list2. This will
   be zero if list1 and list2 are equal, and will be higher the 'more different'
   list1 is from list2. The calculation is equivalent to the following:
   1. Construct a list of all of the unique items in both of the lists. Sublists 
   and atoms all count as items.                               
   2. Initialize the result to zero.
   3. For each unique item increment the result by the difference between the
   number of occurrences of the item in list1 and the number of occurrences
   of the item in list2.
   4. Return the result."
  [list1 list2]
  (reduce +' (vals (merge-with (comp math/abs -)
                               (frequencies (all-items list1))
                               (frequencies (all-items list2))))))

(defn overlap
  [thing1 thing2]
  "Returns a measure of the similarity of the arguments, which may be
   nested sequences. The overlap is defined in terms of the collections of
   the items contained in each of the arguments, including nested items at
   all levels. The overlap is then the maximal number of pairings by identity
   across the two collections, divided by the size of the larger collection.
   The returned value will range from 0 (for entirely distinct arguments)
   to 1 (for identical arguments). Run (overlap-demo) to see some examples."
  (let [items1 (all-items thing1)
        items2 (all-items thing2)
        freq1 (frequencies items1)
        freq2 (frequencies items2)]
    (/ (apply +'
              (vals (merge-with min
                                (select-keys freq1 (keys freq2))
                                (select-keys freq2 (keys freq1)))))
       (max (count items1) (count items2)))))

(defn overlap-demo
  "Prints some demo output to demonstrate the behavior of the overlap function.             
   The overlap function returns ratios, but these are printed as floating point                
   numbers in the demo output."
  []
  (doall (map (fn [[x y]] (print x "," y "--- float overlap:" (float (overlap x y)) "\n"))
              '((a ())
                   (a a)
                   (a b)
                   (a (a b))
                   (a (a a))
                   ((a) (a b))
                   ((a b) (a c))
                   ((a b) (a b c))
                   ((a b c) (a b d))
                   ((a b c d) (a b c e))
                   ((a b c d) (d c b a))
                   ((a b c d e f g) (a b c d e f h))
                   ((a b) (a b c d e f))
                   ((a (b (c))) (a (b (c))))
                   ((a (b (c))) (a (b (x))))
                   ((a (b (c))) (a (x (c))))
                   ((a (b (c))) (x (b (c))))
                   ((a (b c) (d (e f))) ((d (e f)) a))
                   ((a (b c) (d (e f))) (a a (b c) (d (e f))))
                   )))
  (println :end-of-demo))

(define-registered 
  code_discrepancy
  (fn [state]
    (if (not (empty? (rest (:code state))))
      (push-item (discrepancy (stack-ref :code 0 state) (stack-ref :code 1 state))
                 :integer
                 (pop-item :code (pop-item :code state)))
      state)))

(define-registered 
  code_overlap
  (fn [state]
    (if (not (empty? (rest (:code state))))
      (push-item (float (overlap (stack-ref :code 0 state) (stack-ref :code 1 state)))
                 :float
                 (pop-item :code (pop-item :code state)))
      state)))
