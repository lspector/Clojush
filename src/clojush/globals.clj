(ns clojush.globals)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   globals
;; The values def'ed here tend to remain constant over all runs. The atoms
;; may change depending on the problem. The only values and atoms in this
;; file should be those that are used by Push instructions; all others, with
;; few exceptions, should be defined in push-argmap in pushgp.clj and should
;; be passed to whatever functions use them as arguments.

;; push-types is the list of stacks used by the Push interpreter
(def push-types '(:exec :integer :float :code :boolean :string :zip
                        :tag :auxiliary :return :environment)) ;; Stack types

;; These definitions are used by instructions to keep computed values within limits
;; or when using random instructions.
(def max-number-magnitude 1000000000000) ;; Used by keep-number-reasonable as the maximum size of any integer or float
(def min-number-magnitude 1.0E-10) ;; Used by keep-number-reasonable as the minimum magnitude of any float
(def max-string-length 500) ;; Used by string instructions to ensure that strings don't get too large
(def min-random-integer -10) ;; The minumum value created by the integer_rand instruction
(def max-random-integer 10) ;; The maximum value created by the integer_rand instruction
(def min-random-float -1.0) ;; The minumum value created by the float_rand instruction
(def max-random-float 1.0) ;; The maximum value created by the float_rand instruction
(def min-random-string-length 1) ;; The minimum length of string created by the string_rand instruction
(def max-random-string-length 10) ;; The maximum length of string created by the string_rand instruction
(def max-points-in-random-expressions 50) ;; The maximum length of code created by the string_rand instruction


;;-------DONE THROUGH HERE

;; These are used to save and print the ancestors of each individual



;; The following globals may be reset by arguments to pushgp or other systems that use Push.
(def global-atom-generators (atom ())) ;; the defalult for this will be set below
(def global-max-points (atom 100))
(def global-evalpush-limit (atom 150))
(def global-evalpush-time-limit (atom 0)) ;; in nanoseconds, 0 => no time limit
(def global-node-selection-method (atom :unbiased))
(def global-node-selection-leaf-probability (atom 0.1))
(def global-node-selection-tournament-size (atom 2))
(def global-pop-when-tagging (atom true))
(def global-reuse-errors (atom true))
(def global-use-rmse (atom false))
(def global-use-single-thread (atom false))
(def global-tag-limit (atom 10000))
(def global-uniform-crossover-parameters (atom {:self 0.9 :other 0.2}))
(def global-hybridization-parameters (atom {:self 0.9 :other 0.2}))
(def global-print-timings (atom false))
(def global-timer (atom 0))
(def global-timing-map (atom {:initialization 0 :reproduction 0 :report 0 :fitness 0 :other 0}))
(def global-use-bushy-code (atom false))
(def global-use-ultra-no-paren-mutation (atom false)) ;When true, ULTRA will use no-paren mutation, which means that parentheses won't be added or deleted during mutation.

;;;;;;;;;DONE THROUGH HERE

(def global-print-history (atom false)) ;; histories are lists of total-error values for ancestors
(def global-print-cosmos-data (atom false)) ;; When true, prints COSMOS data

;; Historically-assessed hardness (http://hampshire.edu/lspector/pubs/kleinspector-gptp08-preprint.pdf)
;; using the "Previous Generation / Difference" method. 
(def global-use-historically-assessed-hardness (atom false))
(def solution-rates (atom (repeat 0)))

;; Lexicase Parent Selection (see Spector paper in GECCO-UP 2012 workshop proceedings)
(def global-use-lexicase-selection (atom false)) 

;; Elitegroup lexicase selection (will only work if lexicase-selection is off)
(def elitegroups (atom ()))
(def global-use-elitegroup-lexicase-selection (atom false))

;; Special defs not used by Push instructions, but still need to be globally def'ed, go here.
(def global-top-level-push-code (atom true)) ;; When true, run-push will push the program's code onto the code stack prior to running
(def global-top-level-pop-code (atom true)) ;; When true, run-push will pop the code stack after running the program
(def global-maintain-ancestors (atom false)) ;; if true, save all ancestors in each individual (costly)
