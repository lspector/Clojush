;; smallvariant.clj
;; A problem for Clojush, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2015

;; NOTE: "Success" is not possible on this problem, unless a non-default
;; error threshold is used.

;; This problem exists to evolve small genomes that are recursively variant
;; in the context of autoconstruction. The single error value is the size
;; of the genome + 1 if the genome is recursively variant, or a large
;; penalty otherwise.

(ns clojush.problems.synthetic.smallvariant
  (:use [clojush.args]
        [clojush.pushgp.pushgp]
        [clojush.util]
        [clojush.pushgp.genetic-operators]))

(defn smallvariant-error
  "Returns a vector containing a single error which will be the 
  size of the genome + 1 if the genome is recursively variant, or a large
  penalty otherwise." 
  [individual]
  (if (recursively-variant? (:genome individual) @push-argmap)
    [(inc (count (:genome individual)))]
    [1000000]))

(def argmap
  {:error-function smallvariant-error
   :atom-generators []
   :autoconstructive true
   :report-simplifications 0
   :pass-individual-to-error-function true
   :max-points 500
   :evalpush-limit 1000})




