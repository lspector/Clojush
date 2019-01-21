(ns clojush.ns) 

;; provides a macro that uses all clojush namespaces except for examples
;; and experimental
  
(defmacro use-clojush
  []
  '(do
     (use '(clojush 
             args evaluate globals individual interpreter meta-errors pushstate random 
             simplification translate util))
     (use '(clojush.instructions 
             boolean code common numbers random-instructions environment string char vectors 
             tag zip input-output genome gtm))
     (use '(clojush.pushgp breed genetic-operators pushgp report))
     (use '(clojush.pushgp.selection 
             selection preselection tournament lexicase epsilon-lexicase 
             elitegroup-lexicase random-threshold-lexicase random-toggle-lexicase 
             randomly-truncated-lexicase implicit-fitness-sharing))))

(run-push '(0 in1 integer_eq
              exec_if ()
              (1 in1 integer_eq
                 exec_if (1)
                 (2 in1 integer_eq
                    exec_if (1 1)
                    (1 1 in1 2 integer_sub exec_do*times
                       (integer_dup 2 integer_yankdup integer_add)))))
          (push-item 0 :input (make-push-state)))
