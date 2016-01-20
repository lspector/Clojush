(ns clojush.midje.translate
  "Tests for [[clojush.translate/translate-plush-genome-to-push-program]] in document it's behavior

  That function takes a [[clojush/indvidual/->individual]]"
  (:require [midje.sweet :refer :all]
            [clojush.translate :refer [translate-plush-genome-to-push-program]]
            [clojush.pushgp.pushgp :refer [reset-globals, push-argmap]]
            [clojush.problems.demos.odd-with-uniform-silence-mutation :refer [argmap]]))

(with-state-changes [(before :contents (reset-globals argmap))]
  ; choose two instructions to use in our tests. It doesn't
  ; matter what they are, as long as one operates on the exec stack
  ; and the other doesn't
  (let [exec 'exec_dup
        instr 'code_frominteger]
    (tabular
     (fact "translate-plush-genome-to-push-program"
           (translate-plush-genome-to-push-program {:genome ?plush} @push-argmap) => ?push)
     ?plush ?push

     []                                     '()
     [{:instruction instr, :silent true}]   '()
     [{:instruction exec, :silent true}]    '()


     [{:instruction exec},
      {:instruction instr}]                 (list exec (list instr))

     [{:instruction exec, :close 1},
      {:instruction instr}]                 (list exec, '(), instr)

     [{:instruction exec},
      {:instruction instr, :close 1}
      {:instruction instr}]                 (list exec, (list instr), instr)

     [{:instruction exec}]                  (list exec '())


     [{:instruction instr},
      {:instruction instr},
      {:instruction exec}]                  (list instr, instr, exec '())


     [{:instruction exec},
      {:instruction exec},
      {:instruction instr}]                  (list exec, (list exec (list instr)))

     [{:instruction exec},
      {:instruction exec :close 2},
      {:instruction instr}]                  (list exec, (list exec, '() ), instr)

     [{:instruction instr :close 1}]         instr


     )))
