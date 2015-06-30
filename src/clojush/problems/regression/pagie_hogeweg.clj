; pagie_hogeweg.clj
;
; The Pagie-Hogeweg problem without a float ERC; instead, just
; uses the constant 1.0 as an atom generator.

(ns clojush.problems.regression.pagie-hogeweg
  (:use clojush.pushgp.pushgp)
  (:require clojush.problems.regression.pagie-hogeweg-with-erc))

(def argmap
  (merge clojush.problems.regression.pagie-hogeweg-with-erc/argmap
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