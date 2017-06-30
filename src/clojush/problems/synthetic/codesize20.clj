;; codesize20.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2011

(ns clojush.problems.synthetic.codesize20
  (:require [clojure.math.numeric-tower :as math])
  (:use [clojush.pushgp.pushgp]
        [clojush.pushstate]
        [clojush.interpreter]
        [clojush.util]
        [clojush.instructions.tag]
        [clojush.experimental.tagged-code-macros]))

;; This simple problem serves only to demonstrate tagged-code macros.
;; We seek a piece of code of exactly 20 points, tagged so that it is retrieved
;; when we look for something with tag 0. We provide only instructions to tag and
;; retrieve tagged code and a few tagged-code macros. Because the problem is so
;; easy the demo run uses a small population size.

(defn codesize20-error
  "Runs the provided push program with no inputs. If the resulting tag space
   is empty then this returns a sequence containing a penalty value. Otherwise it 
   returns a sequence containing the difference between 20 and the number of points
   in the code produced by a query for tag 0 ." 
  [individual]
  (assoc individual
         :errors
         (let [pushstate (run-push (:program individual) (make-push-state))]
           (if (empty? (:tag pushstate)) 
             (list 1000000 0)
             (list (math/abs (- (count-points (second (closest-association 0 pushstate))) 20)) 0)))))

;; actual run

(def argmap
  {:error-function codesize20-error
   :atom-generators (list
                      ;; allow tagging of items on the exec stack
                      (tag-instruction-erc [:exec] 1000) 
                      ;; allow retrieval of tagged items to the exec stack
                      (tagged-instruction-erc 1000)
                      ;; allow code_append tag macros (taking 2 arguments and returning 1 result)
                      (tagged-code-macro-erc 'code_append 1000 2 1) 
                      ;; allow code_subst tag macros (taking 3 arguments and returning 1 result)
                      (tagged-code-macro-erc 'code_subst 1000 3 1)
                      ;; allow code_wrap tag macros (taking 1 argument and returning 1 result)
                      (tagged-code-macro-erc 'code_wrap 1000 1 1)
                      )
   :tag-limit 1000
   :population-size 100
   })


;; an evolved solution, passed to the abbreviating function
#_(abbreviate-tagged-code-macros
    '((tag_exec_962 (({:tagged_code_macro true, :instruction code_subst, :argument_tags (912 545 805), :result_tags (140)} 
                                          ({:tagged_code_macro true, :instruction code_append, :argument_tags (859 545), :result_tags (829)})) 
                                          {:tagged_code_macro true, :instruction code_wrap, :argument_tags (723), :result_tags (291)})
                    ({:tagged_code_macro true, :instruction code_wrap, :argument_tags (74), :result_tags (941)} tagged_322 
                                         {:tagged_code_macro true, :instruction code_wrap, :argument_tags (201), :result_tags (404)})) 
                    ({:tagged_code_macro true, :instruction code_append, :argument_tags (943 327), :result_tags (15)})))


;; the result of the call above
;; ((tag_exec_962 ((TCM_code_subst_(912 545 805)_(140) (TCM_code_append_(859 545)_(829))) TCM_code_wrap_(723)_(291)) 
;;    (TCM_code_wrap_(74)_(941) tagged_322 TCM_code_wrap_(201)_(404))) (TCM_code_append_(943 327)_(15)))
