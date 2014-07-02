; pagie_hogeweg_no_erc.clj
;
; The Pagie-Hogeweg problem without a float ERC; instead, just
; uses the constant 1.0 as an atom generator.

(ns clojush.problems.regression.pagie-hogeweg
  (:use clojush.pushgp.pushgp)
  (:require clojush.examples.pagie-hogeweg))

(def argmap
  (merge clojush.examples.pagie-hogeweg/argmap
         {:atom-generators (list 1.0
                                 'x
                                 'y
                                 'float_div
                                 'float_mult
                                 'float_add
                                 'float_sub
                                 )
          :uniform-mutation-constant-tweak-rate 0.0
          }))