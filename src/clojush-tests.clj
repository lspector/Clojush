;; This is a file of commented-out informal tests (without outputs...) 
;; of functions in coljush.clj.

(in-ns 'clojush)

;(println (random-element '(a b c d e)))
;(println (shuffle '(a b c d e)))
;(println (decompose 20 6))
;(println (shuffle (decompose 20 6)))
;(println (random-code-with-size 20 '(1 2 3)))
;(println (random-code-with-size 20 (list 3.14 'squid)))
;(println (random-code-with-size 20 (list 3.14 'squid (fn [] (rand-int 100)))))
;(println (random-code 100 (list 3.14 'squid (fn [] (rand-int 100)))))

#_(time (def p
	   (for [i (range 1000)]
	     (random-code 100 (list 'a 1)))))

;(time (reduce + (map count (map ensure-list p))))
	
;(println (count-points '((this) program (contains (9 points))))) 

;(time (println (reduce + (map count-points p))))

#_(time (reduce + (for [i (range 100)]
		  (let [lst (for [j (range 1000)] j)]
		    (first (shuffle lst))))))

;(println (keep-number-reasonable 10E100))
;(println (keep-number-reasonable -312987231987329187329187321987231987))
;(println (count-points '((this) program (contains (9 points)))))

;(dotimes [i 10] (println (code-at-point '(a (b c) d) i)))
;(println '---)
;(dotimes [i 10] (println (insert-code-at-point '(a (b c) d) i 'x)))
;(println '---)
;(dotimes [i 10] (println (remove-code-at-point '(a (b c) d) i)))
;(println '---)
;(dotimes [i 10] (println (remove-code-at-point '(a (b c (e)) d) i)))
;(println '---)
;(dotimes [i 10] (println (remove-code-at-point '(a (b c (e f)) d) i)))

;(println (make-push-state))

;(register-instruction 'foo)
;(register-instruction 'bar)
;(println registered-instructions)

;(println (macroexpand-1 '(define-registered schmoo (fn [] (println 'hello)))))

;(define-registered schmoo (fn [] (println 'hello)))

;(println registered-instructions)
;(println instruction-table)
;(schmoo)

;(println (get-stack :integer (make-push-state)))

;(println (state-pretty-print (make-push-state)))

;(get-stack 'integer (make-schush-state))    

#_(let [s (make-push-state)]
  (println (push-item 'froggy :code s)))

;(println (top-item :code (push-item 'froggy :code (make-push-state))))

;(println (top-item :integer (make-push-state)))

#_(println (stack-ref :integer 
		    1 
		    (push-item 2 
			  :integer
			  (push-item 1 
				:integer
				(push-item 0
				      :integer
				      (make-push-state))))))

#_(loop [n 0
       state (make-push-state)]
  (if (> n 4)
    (do (println state)
	(println (pop-item :integer state)))
    (recur (inc n) 
	   (push-item n :integer state))))

;(define-registered integer.schmoo (fn [] 0))
;(define-registered float.schmoo (fn [] 0))
;(println (registered-for-type :integer))

;(println ((popper :integer) (push-item 2 :integer (push-item 3 :integer (make-push-state)))))

;(println (->> (make-push-state) (push-item 23 :integer) (push-item 100 :integer) (integer_pop)))

;(println (->> (make-push-state) (push-item 23 :integer) (integer_dup)))

;(println (->> (make-push-state) (push-item 1 :integer) (push-item 2 :integer)))
;(println (->> (make-push-state) (push-item 1 :integer) (push-item 2 :integer) (integer_swap)))
;(println (->> (make-push-state) (push-item 1 :integer) (integer_swap)))

#_(println (->> (make-push-state)
	      (push-item 'a :code)
	      (push-item 'b :code)
	      (push-item 'c :code)
	      (code_rot)))

#_(println (->> (make-push-state)
	      (push-item 'a :code)
	      (push-item 'b :code)
	      (push-item 'c :code)
	      (code_flush)))

#_(println (->> (make-push-state)
	      (push-item 'a :code)
	      (push-item 'b :code)
	      (code_eq)))

#_(println (->> (make-push-state)
	      (push-item 'a :code)
	      (push-item 'a :code)
	      (code_eq)))

#_(println (->> (make-push-state)
	      (push-item 'a :code)
	      (code_eq)))

#_(println (->> (make-push-state)
	      (push-item 'a :code)
	      (push-item 'b :code)
	      (code_stackdepth)))

#_(println (->> (make-push-state)
	      (push-item 'a :code)
	      (push-item 'b :code)
	      (push-item 'c :code)
	      (push-item 'd :code)
	      (push-item 2 :integer)
	      (code_yank)))

#_(println (->> (make-push-state)
	      (push-item 101 :integer)
	      (push-item 102 :integer)
	      (push-item 103 :integer)
	      (push-item 104 :integer)
	      (push-item 2 :integer)
	      (integer_yank)))

#_(println (->> (make-push-state)
	      (push-item 'a :code)
	      (push-item 'b :code)
	      (push-item 'c :code)
	      (push-item 'd :code)
	      (push-item 2 :integer)
	      (code_yankdup)))

#_(println (->> (make-push-state)
	      (push-item 101 :integer)
	      (push-item 102 :integer)
	      (push-item 103 :integer)
	      (push-item 104 :integer)
	      (push-item 2 :integer)
	      (integer_yankdup)))

;(println (run-push '(1 2 integer_dup) (make-push-state) true))

;(println (run-push '(1 2 integer_add) (make-push-state)))
;(println (run-push '(1 integer_add) (make-push-state)))

;(println (run-push '(100 1 integer_sub) (make-push-state)))

;(println (run-push '(10.0 5.0 float_mult) (make-push-state)))

;(println (run-push '(10.0 6.0 float_div) (make-push-state)))
;(println (run-push '(-10.0 6.0 float_div) (make-push-state)))
;(println (run-push '(10 6 integer_div) (make-push-state)))
;(println (run-push '(-10 6 integer_div) (make-push-state)))
;(println (run-push '(10.0 0.0 float_div) (make-push-state)))
;(println (run-push '(10 0 integer_div) (make-push-state)))

;(println (run-push '((10.0 (5.0 float_mult))) (make-push-state)))

;(println (run-push '(10.0 6.0 float_mod) (make-push-state)))
;(println (run-push '(-10.0 6.0 float_mod) (make-push-state)))
;(println (run-push '(10 6 integer_mod) (make-push-state)))
;(println (run-push '(-10 6 integer_mod) (make-push-state)))
;(println (run-push '(10.0 0.0 float_mod) (make-push-state)))
;(println (run-push '(10 0 integer_mod) (make-push-state)))

;(println (run-push '(10.0 11.0 float_lt) (make-push-state)))
;(println (run-push '(10.0 1.0 float_lt) (make-push-state)))
;(println (run-push '(10.0 11.0 float_gt) (make-push-state)))
;(println (run-push '(10.0 1.0 float_gt) (make-push-state)))

;(println (run-push '(false integer_fromboolean) (make-push-state)))
;(println (run-push '(true integer_fromboolean) (make-push-state)))
;(println (run-push '(integer_fromboolean) (make-push-state)))

;(println (run-push '(false float_fromboolean) (make-push-state)))
;(println (run-push '(true float_fromboolean) (make-push-state)))
;(println (run-push '(float_fromboolean) (make-push-state)))

;(println (run-push '(3.14 integer_fromfloat) (make-push-state)))
;(println (run-push '(3 float_frominteger) (make-push-state)))

;(println (run-push '(1 2 integer_min) (make-push-state)))
;(println (run-push '(2 1 integer_min) (make-push-state)))
;(println (run-push '(1.0 2.0 float_min) (make-push-state)))
;(println (run-push '(2.0 1.0 float_min) (make-push-state)))

;(println (run-push '(1 2 integer_max) (make-push-state)))
;(println (run-push '(2 1 integer_max) (make-push-state)))
;(println (run-push '(1.0 2.0 float_max) (make-push-state)))
;(println (run-push '(2.0 1.0 float_max) (make-push-state)))

;(println (run-push '(3.141592 float_sin) (make-push-state)))
;(println (run-push '(3.141592 float_cos) (make-push-state)))
;(println (run-push '(3.141592 float_tan) (make-push-state)))

;(println (run-push '(true false boolean_and) (make-push-state)))
;(println (run-push '(true true boolean_and) (make-push-state)))
;(println (run-push '(true boolean_and) (make-push-state)))
;(println (run-push '(true false boolean_or) (make-push-state)))
;(println (run-push '(false false boolean_or) (make-push-state)))
;(println (run-push '(true boolean_or) (make-push-state)))
;(println (run-push '(true boolean_not) (make-push-state)))

;(println (run-push '(0.0 boolean_fromfloat) (make-push-state)))
;(println (run-push '(10.0 boolean_fromfloat) (make-push-state)))
;(println (run-push '(0 boolean_frominteger) (make-push-state)))
;(println (run-push '(10 boolean_frominteger) (make-push-state)))



;(dotimes [_ 100]
#_(println (let [c (random-code 100 (concat registered-instructions 
					    (list (fn [] (- (rand 2) 1)) 
					       (fn [] (- (rand-int 20) 10)))))]
	   (println c)
	   (run-push c
		     (make-push-state)
		     true
		     )))
;)

(comment
  (defn new-pgm 
    []
    (random-code 100 (concat registered-instructions
			     (list (fn [] (- (rand 2) 1))
				   (fn [] (- (rand-int 20) 10))))))
  
  (def population (doall (for [i (range 1000)] (agent ['(), -1]))))

  (defn print-incomplete 
    []
    (printf "\nIncomplete: %s\n" (reduce + (map #(if (< (nth % 1) 0) 1 0)
						(map deref population)))))

  (time
   (do
     (print-incomplete)
     (dorun (map #(send % (fn [[p f]] [(new-pgm) f])) population)) 
     (apply await population)
     (dorun (map #(send % (fn [[p f]] [p (count (:integer (run-push p (make-push-state))))])) population))
     (apply await population)
     (print-incomplete)
     ))
  )

;;;;;;;;;;;;
;; Integer symbolic regression of x^3 - 2x^2 - x (problem 5 from the trivial geography chapter) with 
;; minimal integer instructions and an input instruction that uses the auxiliary stack.

(comment
(define-registered in (fn [state] (push-item (stack-ref :auxiliary 0 state) :integer state)))

(pushgp {
	 :error-function 
	 (fn [program]
	   (doall
	    (for [input (range 10)]
	      (let [state (run-push program 
				    (push-item input :auxiliary 
					       (push-item input :integer
							  (make-push-state))))
		    top-int (top-item :integer state)]
		(if (number? top-int)
		  (math/abs (- top-int (- (* input input input) (* 2 input input) input)))
		  1000)))))
          :atom-generators (list (fn [] (rand-int 10))
				 'in
				 'integer_div
				 'integer_mult
				 'integer_add
				 'integer_sub)
	 })
)

;;;;;;;;;;;;
;; Integer symbolic regression of factorial, using an input instruction and lots of
;; other instructions. Hard but solvable. 

(comment

(define-registered in (fn [state] (push-item (stack-ref :auxiliary 0 state) :integer state)))

(defn factorial 
  [n]
  ;; Returns the factorial of n. 
  (if (< n 2)
    1
    (* n (factorial (- n 1)))))

(pushgp {:error-function (fn [program]
			   (doall
			    (for [input (range 1 6)]
			      (let [state (run-push program
						    (push-item input :auxiliary
							       (push-item input :integer
									  (make-push-state))))
				    top-int (top-item :integer state)]
				(if (number? top-int)
				  (math/abs (- top-int (factorial input)))
				  1000000000))))) ;; big penalty, since errors can be big
	 :atom-generators (concat (registered-for-type :integer)
				  (registered-for-type :exec)
				  (registered-for-type :boolean)
				  (list (fn [] (rand-int 100))
					'in))
	 :max-points 100
	 :population-size 10000
	 :reproduction-simplifications 2})
)

(comment
(let [population (into [] (for [_ (range 1000)] (struct-map individual :program (random-code 100 '(a b c)) 
							  :total-error (rand-int 100))))]
  (time (dotimes [_ 10000] (select population 7 0 0))))
)

(comment
(println (->> (make-push-state)
	      (push-item 'a :code)
	      (push-item 'b :code)
	      (push-item 'c :code)
	      (push-item 'd :code)
	      (push-item 1 :integer)
	      (code_shove)
	      ))

(println (->> (make-push-state)
	      (push-item 'a :code)
	      (push-item 'b :code)
	      (push-item 'c :code)
	      (push-item 'd :code)
	      (push-item 3 :integer)
	      (code_shove)
	      ))

(println (->> (make-push-state)
	      (push-item 'a :code)
	      (push-item 'b :code)
	      (push-item 'c :code)
	      (push-item 'd :code)
	      (push-item 55 :integer)
	      (code_shove)
	      ))

(println (->> (make-push-state)
	      (push-item 'a :code)
	      (push-item 'b :code)
	      (push-item 'c :code)
	      (push-item 'd :code)
	      (push-item -2 :integer)
	      (code_shove)
	      ))

(println (->> (make-push-state)
	      (push-item 101 :integer)
	      (push-item 102 :integer)
	      (push-item 103 :integer)
	      (push-item 0 :integer)
	      (integer_shove)
	      ))

(println (->> (make-push-state)
	      (push-item 101 :integer)
	      (push-item 102 :integer)
	      (push-item 103 :integer)
	      (push-item 1 :integer)
	      (integer_shove)
	      ))
) ;; end of shove tests

#_(println (->> (make-push-state)
	      (push-item '(a b c) :code)
	      (push-item 2 :integer)
	      (code_extract)))

#_(println (->> (make-push-state)
	      (push-item '(a b c) :code)
	      (push-item '(x y z) :code)
	      (push-item 2 :integer)
	      (code_insert)))

#_(println (subst 1 2 '(1 2 3)))
#_(println (subst '(a b) '(x y) '(1 2 (x y) (3 4 ((x y))) (x y))))

#_(println (->> (make-push-state)
	      (push-item '(1 2 3) :code)
	      (push-item 'b :code)
	      (push-item '(a b (a b (a b) a b)) :code)
	      (code_subst)))

#_(println (contains-subtree '(1 (2 3) 4) 3))
#_(println (contains-subtree '(1 (2 (3 4)) x) '(3 4)))
#_(println (contains-subtree '(1 (2 (3 4)) x) '(2 3)))

#_(println (->> (make-push-state)
	      (push-item '(1 (2 (a b) 3)) :code)
	      (push-item '(a b) :code)
	      (code_contains)))

#_(println (->> (make-push-state)
	      (push-item '(1 (2 (a b) 3)) :code)
	      (push-item '(a) :code)
	      (code_contains)))

#_(println (containing-subtree '(b (c (a)) (d (a))) '(a)) )

#_(println (->> (make-push-state)
	      (push-item '(a) :code)
	      (push-item '(b (c (a)) (d (a))) :code)
	      (code_container)))

#_(println (->> (make-push-state)
	      (push-item 'a :code)
	      (push-item '(x x a x x x a x) :code)
	      (code_position)))

#_(println (->> (make-push-state)
	      (push-item 'b :code)
	      (push-item '(x x a x x x a x) :code)
	      (code_position)))

#_(println (discrepancy '(a b c d) '(a b c d)))
#_(println (discrepancy '(a b c d e) '(a b c d e)))
#_(println (discrepancy '(a b c d e) '(a b c d)))

#_(println (->> (make-push-state)
	      (push-item '(a b c) :code)
	      (push-item '(a b) :code)
	      (code_discrepancy)))

#_(println (->> (make-push-state)
	      (boolean_rand)
	      (integer_rand)
	      (float_rand)
	      (push-item 25 :integer)
	      (code_rand)
	      ))

#_(do (def top-level-push-code false)
    (def top-level-pop-code false)
    (println (run-push '(code_quote (a b c) code_wrap)
		       (make-push-state)))
    (println (run-push '(code_quote (a b c) code_map (code_dup code_list))
		       (make-push-state)))
    (println (run-push '(code_quote a code_map (code_dup code_list))
		       (make-push-state)))
    )

;; factorial example from push3 spec, translated into clojush
#_(def top-level-pop-code false)
#_(println (run-push '(code_quote 
		     (integer_pop 1)
		     code_quote 
		     (code_dup integer_dup 1 integer_sub code_do integer_mult)
		     integer_dup 2 integer_lt code_if)
		   (push-item 5 :integer (make-push-state))))

;; pathological quasiquine
#_(def top-level-push-code false) ;; don't push code initially, must construct
#_(def top-level-pop-code false) ;; don't pop resulting code
#_(println (run-push '(1 9 code_quote (integer_pop code_pop code_quote) code_do*range)
		   (make-push-state)
		   true))

;(println (run-push '(1 2 tag_integer_123) (make-push-state)))

;(println (run-push '(1 2 integer_add tag_integer_123 99 tagged_001) (make-push-state)))
;(println (run-push '(1 2 integer_add tag_integer_123 99 tagged_901) (make-push-state)))
;(println (run-push '(1 2 integer_add tag_integer_123 99 tagged_001 untag_222) (make-push-state)))
;(println (run-push '(1 2 integer_add tag_integer_123 99 tagged_001 untag_222 tagged_123) (make-push-state)))
;(println (run-push '(1 2 integer_add tag_integer_123 99 tagged_001 untag_222 tagged_123 integer_add tag_integer_12) (make-push-state)))
 
;((tag-instruction-erc [:integer :float] 100))

;(let [c '(+ (* 1 2) (/ 3 4))] (code-at-point c (choose-node-index-with-leaf-probability c)))
;(let [c (random-code-with-size 1000 '(1))] (time (dotimes [_ 10] (choose-node-index-with-leaf-probability c))))
;(do (dotimes [_ 1000] (choose-node-index-with-leaf-probability (random-code 100 '(1)))) :no-failures)

;(println (run-push '(1 2 integer_add tag_integer_123 99 tagged_code_001 code_dup) (make-push-state)))
