(ns clojush.globals)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; globals

(def push-types '(:exec :integer :float :code :boolean :auxiliary :tag :zip :string))
(def max-number-magnitude 1000000000000)
(def min-number-magnitude 1.0E-10)
(def top-level-push-code true)
(def top-level-pop-code true)
(def min-random-integer -10)
(def max-random-integer 10)
(def min-random-float -1.0)
(def max-random-float 1.0)
(def min-random-string-length 1)
(def max-random-string-length 10)
(def max-string-length 100)
(def max-points-in-random-expressions 50) ;; for code_rand
(def maintain-histories true) ;; histories are lists of total-error values for ancestors
(def maintain-ancestors false) ;; if true save all ancestors in each individual (costly)
(def print-ancestors-of-solution false)

;; The following globals require values because they are used in Push instructions but they
;; may be reset by arguments to pushgp or other systems that use Push.
(def global-atom-generators (atom ())) ;; the defalult for this will be set below
(def global-max-points-in-program (atom 100))
(def global-evalpush-limit (atom 150))
(def global-evalpush-time-limit (atom 0)) ;; in nanoseconds, 0 => no time limit
(def global-node-selection-method (atom :unbiased))
(def global-node-selection-leaf-probability (atom 0.1))
(def global-node-selection-tournament-size (atom 2))
(def global-pop-when-tagging (atom true))
(def global-reuse-errors (atom true))
(def global-use-single-thread (atom false))

;; Historically-assessed hardness (http://hampshire.edu/lspector/pubs/kleinspector-gptp08-preprint.pdf)
;; using the "Previous Generation / Difference" method. 
(def global-use-historically-assessed-hardness (atom false))
(def solution-rates (atom (repeat 0)))

;; Lexicase Parent Selection (see Spector paper in GECCO-UP 2012 workshop proceedings)
(def global-use-lexicase-selection (atom false)) ;; if true then no other selection params matter
