;; (ns clojush.test.stress_test
;;   (:use [clojush.random]
;;         [clojush.pushstate]
;;         [clojush.interpreter]))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; stress test

;; (defn stress-test
;;   "Performs a stress test of the registered instructions by generating and running n
;;    random programs. For more thorough testing and debugging of Push instructions you many
;;    want to un-comment code in execute-instruction that will allow you to look at recently
;;    executed instructions and the most recent state after an error. That code burns memory,
;;    however, so it is normally commented out. You might also want to comment out the handling
;;    of nil values in execute-instruction, do see if any instructions are introducing nils."
;;   [n]
;;   (let [completely-random-program
;;         (fn []
;;           (random-code 100 (concat @registered-instructions
;;                                    (list (fn [] (lrand-int 100))
;;                                          (fn [] (lrand))))))]
;;     (loop [i 0 p (completely-random-program)]
;;       (if (>= i n)
;;         (println :no-errors-found-in-stress-test)
;;         (let [result (run-push p (make-push-state) false)]
;;           (if result
;;             (recur (inc i) (completely-random-program))
;;             (println p)))))))

;; ;(stress-test 10000)
