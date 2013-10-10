(ns clojush.globals)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; globals

(def push-types '(:exec :integer :float :code :boolean :string :zip
                        :tag :auxiliary :return :environment))
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

;; Historically-assessed hardness (http://hampshire.edu/lspector/pubs/kleinspector-gptp08-preprint.pdf)
;; using the "Previous Generation / Difference" method. 
(def global-use-historically-assessed-hardness (atom false))
(def solution-rates (atom (repeat 0)))

;; Lexicase Parent Selection (see Spector paper in GECCO-UP 2012 workshop proceedings)
(def global-use-lexicase-selection (atom false)) 

;; Elitegroup lexicase selection (will only work if lexicase-selection is off)
(def elitegroups (atom ()))
(def global-use-elitegroup-lexicase-selection (atom false))
