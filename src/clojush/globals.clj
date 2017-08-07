(ns clojush.globals)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The values of the names defined here tend to remain constant over all runs.
;; Those not starting with "global-" are used in a variety of places and therefore
;; it is easiest to keep them global. The names starting with "global-"
;; are bound to atoms for which values may be set by arguments to pushgp;
;; see the definition of push-argmap in args.clj for more information on these.


(def push-types '(:exec :code :integer :float :boolean :char :string :zip
                  :vector_integer :vector_float :vector_boolean :vector_string
                  :input :output :auxiliary :tag :return :environment :genome))
;; The list of stacks used by the Push interpreter


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Used by instructions to keep computed values within limits or when using random
;; instructions.

(def max-number-magnitude 1000000000000)
;; Used by keep-number-reasonable as the maximum size of any integer or float

(def min-number-magnitude 1.0E-10)
;; Used by keep-number-reasonable as the minimum magnitude of any float

(def max-string-length 5000)
;; Used by string instructions to ensure that strings don't get too large

(def max-vector-length 5000)
;; Used by vector instructions to ensure that vectors don't get too large

(def min-random-integer -10)
;; The minumum value created by the integer_rand instruction

(def max-random-integer 10)
;; The maximum value created by the integer_rand instruction

(def min-random-float -1.0)
;; The minumum value created by the float_rand instruction

(def max-random-float 1.0)
;; The maximum value created by the float_rand instruction

(def min-random-string-length 1)
;; The minimum length of string created by the string_rand instruction

(def max-random-string-length 10)
;; The maximum length of string created by the string_rand instruction

(def max-points-in-random-expressions 50)
;; The maximum length of code created by the code_rand instruction


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Used in many places and difficult to make fully functional

(def evaluations-count (atom 0))
;; Used to count the number of times GP evaluates an individual

(def point-evaluations-count (atom 0))
;; Used to count the number of instructions that have been executed

(def solution-rates (atom (repeat 0)))
;; Used in historically-assessed hardness

(def elitegroups (atom ()))
;; Used for elitegroup lexicase selection (will only work if lexicase-selection is off)

(def epsilons-for-epsilon-lexicase (atom ()))
;; Used in epsilon lexicase. Only calculated once per population

(def selection-counts (atom {}))
;; Used to store the number of selections for each individual, indexed by UUIDs

(def min-age (atom 0))
(def max-age (atom 0))
;; Used for age-mediated-parent-selection

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The globals below may be reset by arguments to pushgp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These definitions are used by Push instructions and therefore must be global

(def global-atom-generators (atom ()))
;; The instructions and literals that may be used in Push programs.

(def global-max-points (atom 100))
;; The maximum size of a Push program. Also, the maximum size of code that can appear on
;; the exec or code stacks.

(def global-tag-limit (atom 10000))
;; The size of the tag space

(def global-epigenetic-markers (atom [:close]))
;; A vector of the epigenetic markers that should be used in the individuals. Implemented
;; options include: :close, :silent

(def global-close-parens-probabilities (atom [0.772 0.206 0.021 0.001]))
;; A vector of the probabilities for the number of parens ending at that position. See
;; random-closes in clojush.random

(def global-silent-instruction-probability (atom 0.2))
;; If :silent is used as an epigenetic-marker, this is the probability of random
;; instructions having :silent be true

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These definitions are used by run-push (and functions it calls), and must be global
;; since run-push is called by the problem-specifc error functions

(def global-top-level-push-code (atom false))
;; When true, run-push will push the program's code onto the code stack prior to running

(def global-top-level-pop-code (atom false))
;; When true, run-push will pop the code stack after running the program

(def global-evalpush-limit (atom 150))
;; The number of Push instructions that can be evaluated before stopping evaluation

(def global-evalpush-time-limit (atom 0))
;; The time in nanoseconds that a program can evaluate before stopping, 0 means no time limit

(def global-pop-when-tagging (atom true))
;; When true, tagging instructions will pop the exec stack when tagging; otherwise, the exec
;; stack is not popped

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These definitions are used by some problem-specific error functions, and must therefore
;; be global

(def global-parent-selection (atom :lexicase))
;; The type of parent selection used
