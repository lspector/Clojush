;; gorilla-repl.fileformat = 1

;; **
;;; # Clojush Tutorial
;;; 
;;; Lee Spector, lspector@hampshire.edu
;;; 
;;; This tutorial shows how to use Clojush, a Clojure implementation of the [Push programming language](http://pushlanguage.org) and the PushGP genetic programming system, as a library in your own projects. To download the full system and for instructions for running the examples that are included with it, see the [Clojush github page](https://github.com/lspector/Clojush/tree/c6e9bc5e9835ed3c57703509e5b9fb988ea5f081).
;;; 
;;; This tutorial was created with [Gorilla REPL](http://gorilla-repl.org). To run this worksheet, or to use the code that it contains in some other worksheet or project, you must include the Clojush dependency in your project's `project.clj` file. This tutorial was created with the dependency of [clojush "2.0.60"].	
;;; 
;;; First, we create a namespace for the code that we will run:
;; **

;; @@
(ns tutorial-worksheet)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; These expressions provide access to *all* Clojush namespaces:
;; **

;; @@
(use 'clojush.ns)
(use-clojush)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; Clojush is an implementation of the Push programming for evolutionary computation, and of the PushGP genetic programming system that evolves Push programs.
;;; 
;;; Push programs operate on data stacks, with a separate stack for each data type. 
;;; 
;;; A "Push state" is a complete set of data stacks, and the `make-push-state` function returns a Push state with all stacks empty:
;; **

;; @@
(make-push-state)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:exec</span>","value":":exec"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:exec nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:code</span>","value":":code"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:code nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:integer</span>","value":":integer"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:integer nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:float</span>","value":":float"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:float nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:boolean</span>","value":":boolean"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:boolean nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:char</span>","value":":char"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:char nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:string</span>","value":":string"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:string nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:zip</span>","value":":zip"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:zip nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:vector_integer</span>","value":":vector_integer"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:vector_integer nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:vector_float</span>","value":":vector_float"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:vector_float nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:vector_boolean</span>","value":":vector_boolean"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:vector_boolean nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:vector_string</span>","value":":vector_string"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:vector_string nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:input</span>","value":":input"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:input nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:output</span>","value":":output"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:output nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:auxiliary</span>","value":":auxiliary"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:auxiliary nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:tag</span>","value":":tag"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:tag nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:return</span>","value":":return"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:return nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:environment</span>","value":":environment"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:environment nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:genome</span>","value":":genome"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:genome nil]"}],"value":"#clojush.pushstate.PushState{:exec nil, :code nil, :integer nil, :float nil, :boolean nil, :char nil, :string nil, :zip nil, :vector_integer nil, :vector_float nil, :vector_boolean nil, :vector_string nil, :input nil, :output nil, :auxiliary nil, :tag nil, :return nil, :environment nil, :genome nil}"}
;; <=

;; **
;;; What you see here are all of the data stacks included by default in the current version of Clojush. 
;;; 
;;; You can ignore most of them for now. The `:exec` stack is the most important one, since it stores a program while it is being run. 
;;; 
;;; For the sake of this tutorial we'll only be using `:exec` and a few others, so here's a function that shows us just the ones we want:
;; **

;; @@
(defn stks
  "Return a map with just a few of the stacks in a Push state."
  [push-state]
  (select-keys push-state
               [:exec :code :integer :float :boolean]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tutorial-worksheet/stks</span>","value":"#'tutorial-worksheet/stks"}
;; <=

;; @@
(stks (make-push-state))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:exec</span>","value":":exec"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:exec nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:code</span>","value":":code"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:code nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:integer</span>","value":":integer"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:integer nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:float</span>","value":":float"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:float nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:boolean</span>","value":":boolean"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:boolean nil]"}],"value":"{:exec nil, :code nil, :integer nil, :float nil, :boolean nil}"}
;; <=

;; **
;;; Now let's actually run a Push program. 
;;; 
;;; Push is a stack-based language, which means that when the interpreter sees a value it just pushes it on the appropriate stack, and when it sees an instruction it pops any needed arguments from the stacks, executes the instruction, and pushes results back on the appropriate stacks.
;;; 
;;; Here we run the Push program `(1 2 integer_add)` on an initially empty Push state and return the resulting Push state (with just the stacks we care about):
;; **

;; @@
(stks (run-push '(1 2 integer_add) 
                (make-push-state)))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:exec</span>","value":":exec"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[],"value":"()"}],"value":"[:exec ()]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:code</span>","value":":code"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:code nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:integer</span>","value":":integer"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"(3)"}],"value":"[:integer (3)]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:float</span>","value":":float"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:float nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:boolean</span>","value":":boolean"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:boolean nil]"}],"value":"{:exec (), :code nil, :integer (3), :float nil, :boolean nil}"}
;; <=

;; **
;;; You can see that `3` is on the `:integer` stack at the end because `1+2=3`, and because the program `(1 2 integer_add)` essentially says to first push `1` onto the `:integer` stack, then `2`, and then execute the `integer_add` instruction, which pops two integers and then pushes their sum back on the `:integer` stack.
;;; 
;;; If you give `run-push` a `true` third argument then it will print the (complete) Push state at each execution step. For this simple example we first see the entire program pushed on to the `:exec` stack. Then we see that the interpreter processes the program list (as it processes all lists) by pushing its elements individually back onto the `:exec` stack (notice that a layer of parentheses is removed in the first step). Then we see the `1` being processed (notice that it moves to the `:integer` stack), and then the `2`, and finally `integer_add`. Then the `:exec` stack is empty and execution terminates:
;; **

;; @@
(stks (run-push '(1 2 integer_add) 
                (make-push-state)
                true))
;; @@
;; ->
;;; 
;;; State after 0 steps:
;;; :exec = ((1 2 integer_add))
;;; :code = nil
;;; :integer = nil
;;; :float = nil
;;; :boolean = nil
;;; :char = nil
;;; :string = nil
;;; :zip = nil
;;; :vector_integer = nil
;;; :vector_float = nil
;;; :vector_boolean = nil
;;; :vector_string = nil
;;; :input = nil
;;; :output = nil
;;; :auxiliary = nil
;;; :tag = nil
;;; :return = nil
;;; :environment = nil
;;; :genome = nil
;;; 
;;; State after 1 steps (last step: (...)):
;;; :exec = (1 2 integer_add)
;;; :code = nil
;;; :integer = nil
;;; :float = nil
;;; :boolean = nil
;;; :char = nil
;;; :string = nil
;;; :zip = nil
;;; :vector_integer = nil
;;; :vector_float = nil
;;; :vector_boolean = nil
;;; :vector_string = nil
;;; :input = nil
;;; :output = nil
;;; :auxiliary = nil
;;; :tag = nil
;;; :return = nil
;;; :environment = nil
;;; :genome = nil
;;; 
;;; State after 2 steps (last step: 1):
;;; :exec = (2 integer_add)
;;; :code = nil
;;; :integer = (1)
;;; :float = nil
;;; :boolean = nil
;;; :char = nil
;;; :string = nil
;;; :zip = nil
;;; :vector_integer = nil
;;; :vector_float = nil
;;; :vector_boolean = nil
;;; :vector_string = nil
;;; :input = nil
;;; :output = nil
;;; :auxiliary = nil
;;; :tag = nil
;;; :return = nil
;;; :environment = nil
;;; :genome = nil
;;; 
;;; State after 3 steps (last step: 2):
;;; :exec = (integer_add)
;;; :code = nil
;;; :integer = (2 1)
;;; :float = nil
;;; :boolean = nil
;;; :char = nil
;;; :string = nil
;;; :zip = nil
;;; :vector_integer = nil
;;; :vector_float = nil
;;; :vector_boolean = nil
;;; :vector_string = nil
;;; :input = nil
;;; :output = nil
;;; :auxiliary = nil
;;; :tag = nil
;;; :return = nil
;;; :environment = nil
;;; :genome = nil
;;; 
;;; State after 4 steps (last step: integer_add):
;;; :exec = ()
;;; :code = nil
;;; :integer = (3)
;;; :float = nil
;;; :boolean = nil
;;; :char = nil
;;; :string = nil
;;; :zip = nil
;;; :vector_integer = nil
;;; :vector_float = nil
;;; :vector_boolean = nil
;;; :vector_string = nil
;;; :input = nil
;;; :output = nil
;;; :auxiliary = nil
;;; :tag = nil
;;; :return = nil
;;; :environment = nil
;;; :genome = nil
;;; 
;; <-
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:exec</span>","value":":exec"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[],"value":"()"}],"value":"[:exec ()]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:code</span>","value":":code"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:code nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:integer</span>","value":":integer"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"(3)"}],"value":"[:integer (3)]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:float</span>","value":":float"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:float nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:boolean</span>","value":":boolean"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:boolean nil]"}],"value":"{:exec (), :code nil, :integer (3), :float nil, :boolean nil}"}
;; <=

;; **
;;; To summarize, when the interpreter sees a value it pushes it onto the appropriate stack. When it sees an instruction it pops the needed arguments and pushes the results onto the appropriate stacks. If there are not sufficient arguments on the stacks then the instruction **does nothing**. And when the interpreter sees a list it simply pushes the contents of the list back onto the `:exec` stack individually.
;;; 
;;; That's pretty much all you need to know about how the Push interpreter works. Some instructions can do complicated things, including manipulating code on the `:exec` stack (which can create loops and conditionals, etc.), but the Push interpreter just follows the simple rules sketched here and all instructions take values from stacks and push results from stacks.
;;; 
;;; Here is a slightly more complicated example:
;; **

;; @@
(stks (run-push '(5 1.23 integer_add (4) integer_sub 
                  5.67 float_mult)
                (make-push-state)))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:exec</span>","value":":exec"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[],"value":"()"}],"value":"[:exec ()]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:code</span>","value":":code"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:code nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:integer</span>","value":":integer"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"(1)"}],"value":"[:integer (1)]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:float</span>","value":":float"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>6.9741</span>","value":"6.9741"}],"value":"(6.9741)"}],"value":"[:float (6.9741)]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:boolean</span>","value":":boolean"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:boolean nil]"}],"value":"{:exec (), :code nil, :integer (1), :float (6.9741), :boolean nil}"}
;; <=

;; **
;;; Notice that the integer and float operations are interleaved, that `integer_add` has no effect because there is only one integer on the stack when it is executed, and that the parentheses around the `4` have no effect.
;;; 
;;; For every type, there are standard stack manipulation instructions including `dup` (which duplicates the top element),  `pop` (which throws away the top element), `swap` (which exchanges the top two elements), `rot` (which rotates the top three elements, inserting the top element beneath the next two), and `flush` (which empties the stack).
;;; 
;;; Here's a simple example with `integer_dup`:
;; **

;; @@
(stks (run-push '(5 integer_dup integer_mult)
                (make-push-state)))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:exec</span>","value":":exec"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[],"value":"()"}],"value":"[:exec ()]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:code</span>","value":":code"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:code nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:integer</span>","value":":integer"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>25</span>","value":"25"}],"value":"(25)"}],"value":"[:integer (25)]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:float</span>","value":":float"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:float nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:boolean</span>","value":":boolean"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:boolean nil]"}],"value":"{:exec (), :code nil, :integer (25), :float nil, :boolean nil}"}
;; <=

;; **
;;; These instructions work on the `:exec` stack too, which lets us do things like this:
;; **

;; @@
(stks (run-push '(5 10 20 exec_dup integer_mult)
                (make-push-state)))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:exec</span>","value":":exec"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[],"value":"()"}],"value":"[:exec ()]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:code</span>","value":":code"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:code nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:integer</span>","value":":integer"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1000</span>","value":"1000"}],"value":"(1000)"}],"value":"[:integer (1000)]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:float</span>","value":":float"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:float nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:boolean</span>","value":":boolean"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:boolean nil]"}],"value":"{:exec (), :code nil, :integer (1000), :float nil, :boolean nil}"}
;; <=

;; **
;;; This might be confusing, because while most Push instructions operate on the things that come before them in the program (looking like "postfix" operations), the `exec_dup` instruction looks like it operates on something that comes after it (like a "prefix" operation). 
;;; 
;;; But this makes sense when you consider how the Push interpreter executes programs, using the `:exec` stack: when `exec_dup` is being executed, `integer_mult` will be on top of the `:exec` stack, so that is what will be duplicated (and subsequently executed).
;;; 
;;; Now we can also see how parentheses can matter for program execution:
;; **

;; @@
(stks (run-push '(5 exec_dup (3 integer_mult))
                (make-push-state)))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:exec</span>","value":":exec"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[],"value":"()"}],"value":"[:exec ()]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:code</span>","value":":code"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:code nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:integer</span>","value":":integer"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>45</span>","value":"45"}],"value":"(45)"}],"value":"[:integer (45)]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:float</span>","value":":float"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:float nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:boolean</span>","value":":boolean"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:boolean nil]"}],"value":"{:exec (), :code nil, :integer (45), :float nil, :boolean nil}"}
;; <=

;; **
;;; What's going on here is that `exec_dup` duplicates the entire `(3 integer_mult)` expression, so that whole thing ends up getting executed twice.
;;; 
;;; Manipulating the `:exec` stack provides a lot of power, because a program can grow or change itself as it runs.
;;; 
;;; Even more fancy code-self-manipulation stuff can be done using the `:code` stack, for which there are many high-level list-processing instructions.
;;; 
;;; Here's a simple example:
;; **

;; @@
(stks (run-push '(code_quote 20 code_quote 2 code_quote 
                  (2 3 integer_mult) code_subst 
                  code_do)
                (make-push-state)))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:exec</span>","value":":exec"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[],"value":"()"}],"value":"[:exec ()]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:code</span>","value":":code"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"}],"value":"[:code ()]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:integer</span>","value":":integer"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>60</span>","value":"60"}],"value":"(60)"}],"value":"[:integer (60)]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:float</span>","value":":float"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:float nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:boolean</span>","value":":boolean"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:boolean nil]"}],"value":"{:exec (), :code (), :integer (60), :float nil, :boolean nil}"}
;; <=

;; **
;;; What happened here is that `code_subst` substituted `20` for `2` in a piece of code, `(2 3 integer_mult)`, and then `code_do` executed it (by copying it to the `:exec` stack). This is a pretty silly example, but it demonstrates how code it's possible to manipulate code and then execute the manipulated code.
;;; 
;;; Some instructions may take data from multiple stacks, or push results onto multiple stacks. A simple example of the former are the `eq` instructions, which are implemented for many types. These take two items of the specified type and push `true` on the `:boolean` stack if they are equal, or `false` if they are not:
;; **

;; @@
(stks (run-push '(1 2 integer_eq)
                (make-push-state)))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:exec</span>","value":":exec"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[],"value":"()"}],"value":"[:exec ()]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:code</span>","value":":code"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:code nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:integer</span>","value":":integer"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"}],"value":"[:integer ()]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:float</span>","value":":float"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:float nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:boolean</span>","value":":boolean"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>false</span>","value":"false"}],"value":"(false)"}],"value":"[:boolean (false)]"}],"value":"{:exec (), :code nil, :integer (), :float nil, :boolean (false)}"}
;; <=

;; @@
(stks (run-push '(1 1 integer_eq)
                (make-push-state)))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:exec</span>","value":":exec"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[],"value":"()"}],"value":"[:exec ()]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:code</span>","value":":code"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:code nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:integer</span>","value":":integer"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"}],"value":"[:integer ()]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:float</span>","value":":float"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:float nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:boolean</span>","value":":boolean"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"}],"value":"(true)"}],"value":"[:boolean (true)]"}],"value":"{:exec (), :code nil, :integer (), :float nil, :boolean (true)}"}
;; <=

;; **
;;; What can you do with those boolean values? Well, there are boolean instructions like `boolean_and`, `boolean_or`, and `boolean_not`, but there are also handy things like `exec_if`:
;; **

;; @@
(stks (run-push '(1 2 integer_eq exec_if 123 99.6)
                (make-push-state)))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:exec</span>","value":":exec"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[],"value":"()"}],"value":"[:exec ()]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:code</span>","value":":code"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:code nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:integer</span>","value":":integer"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"}],"value":"[:integer ()]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:float</span>","value":":float"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>99.6</span>","value":"99.6"}],"value":"(99.6)"}],"value":"[:float (99.6)]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:boolean</span>","value":":boolean"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"}],"value":"[:boolean ()]"}],"value":"{:exec (), :code nil, :integer (), :float (99.6), :boolean ()}"}
;; <=

;; **
;;; What happened there? After executing `integer_eq` there was a `false` on top of the `:boolean` stack. The `exec_if` instruction takes the top element of the `:boolean` stack and if it's false then it also removes the top element of the `:exec` stack (which in this case was `123`). If it's `true` then it leaves the top element of the `:exec` stack but removes the *second* element. This means that only one of them will be executed, depending on the answer of the previous boolean expression. Here each branch of the conditional expression was just a single number (one, an integer, and the other, a float), but you could just as well have complex programs in each branch, in parentheses.
;;; 
;;; Just for kicks, and to show that it can be done, here's a pretty messy program that computes the factorial of a number (`8` in the example) using recursion on the `:code` stack.
;;; 
;; **

;; @@
(stks (run-push '(code_quote 
                   (code_quote (integer_pop 1) 
                    code_quote (code_dup integer_dup  
                                1 integer_sub code_do 
                                integer_mult)
                    integer_dup 2 integer_lt code_if)
                  code_dup
                  8
                  code_do)
                (make-push-state)))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:exec</span>","value":":exec"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[],"value":"()"}],"value":"[:exec ()]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:code</span>","value":":code"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>code_quote</span>","value":"code_quote"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>integer_pop</span>","value":"integer_pop"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"(integer_pop 1)"},{"type":"html","content":"<span class='clj-symbol'>code_quote</span>","value":"code_quote"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>code_dup</span>","value":"code_dup"},{"type":"html","content":"<span class='clj-symbol'>integer_dup</span>","value":"integer_dup"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-symbol'>integer_sub</span>","value":"integer_sub"},{"type":"html","content":"<span class='clj-symbol'>code_do</span>","value":"code_do"},{"type":"html","content":"<span class='clj-symbol'>integer_mult</span>","value":"integer_mult"}],"value":"(code_dup integer_dup 1 integer_sub code_do integer_mult)"},{"type":"html","content":"<span class='clj-symbol'>integer_dup</span>","value":"integer_dup"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-symbol'>integer_lt</span>","value":"integer_lt"},{"type":"html","content":"<span class='clj-symbol'>code_if</span>","value":"code_if"}],"value":"(code_quote (integer_pop 1) code_quote (code_dup integer_dup 1 integer_sub code_do integer_mult) integer_dup 2 integer_lt code_if)"}],"value":"((code_quote (integer_pop 1) code_quote (code_dup integer_dup 1 integer_sub code_do integer_mult) integer_dup 2 integer_lt code_if))"}],"value":"[:code ((code_quote (integer_pop 1) code_quote (code_dup integer_dup 1 integer_sub code_do integer_mult) integer_dup 2 integer_lt code_if))]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:integer</span>","value":":integer"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>40320</span>","value":"40320"}],"value":"(40320)"}],"value":"[:integer (40320)]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:float</span>","value":":float"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:float nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:boolean</span>","value":":boolean"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"}],"value":"[:boolean ()]"}],"value":"{:exec (), :code ((code_quote (integer_pop 1) code_quote (code_dup integer_dup 1 integer_sub code_do integer_mult) integer_dup 2 integer_lt code_if)), :integer (40320), :float nil, :boolean ()}"}
;; <=

;; **
;;; How does it work? It first pushes a bunch of stuff (let's call this the *recursive definition*) onto the `:code` stack, then duplicates it, then pushes `8` onto the `:integer` stack, and then finally executes `code_do` which will move the top copy of the recursive definition from the `:code` stack to the `:exec` stack, from where it will then be executed. I won't walk through the rest of the process in full detail, but the key thing to notice is that the recursive definition first pushes two bodies of code to the `:code` stack, one for the "base case" of the recursion and one for the "recursive case," and that it then compares the top `:integer` to 2 to decide which of the branches to execute. You can also notice that the first thing in the recursive branch is another call to `code_dup`, which ensures that there will be another copy of the recursive definition on the `:code` stack for further recursion if necessary.
;;; 
;;; This provides an example of recursion on the `:code` stack, but happily, there are also much simpler ways to compute factorials in Push, for example:
;;; 
;;; 
;; **

;; @@
(stks (run-push '(1 8 exec_do*range integer_mult)
                (make-push-state)))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:exec</span>","value":":exec"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[],"value":"()"}],"value":"[:exec ()]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:code</span>","value":":code"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:code nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:integer</span>","value":":integer"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>40320</span>","value":"40320"}],"value":"(40320)"}],"value":"[:integer (40320)]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:float</span>","value":":float"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:float nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:boolean</span>","value":":boolean"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:boolean nil]"}],"value":"{:exec (), :code nil, :integer (40320), :float nil, :boolean nil}"}
;; <=

;; **
;;; This example uses `exec_do*range`, which is an instruction that loops a body of code that it finds on the `:exec` stack, pushing a counter onto the integer stack before each loop body execution. There are other, related functions too, like `exec_do*count`, `exec_while`, etc.
;;; 
;;; Since we can do looping and recursion, it's possible to produce code that will run for a very long time, possibly infinitely. Push handles this by imposing an execution step limit:
;; **

;; @@
(stks (run-push '(0 true exec_while 
                  (1 integer_add true))
                (make-push-state)))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:exec</span>","value":":exec"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-symbol'>integer_add</span>","value":"integer_add"},{"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"},{"type":"html","content":"<span class='clj-symbol'>exec_while</span>","value":"exec_while"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-symbol'>integer_add</span>","value":"integer_add"},{"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"}],"value":"(1 integer_add true)"}],"value":"(1 integer_add true exec_while (1 integer_add true))"}],"value":"[:exec (1 integer_add true exec_while (1 integer_add true))]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:code</span>","value":":code"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:code nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:integer</span>","value":":integer"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>29</span>","value":"29"}],"value":"(29)"}],"value":"[:integer (29)]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:float</span>","value":":float"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:float nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:boolean</span>","value":":boolean"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"}],"value":"[:boolean ()]"}],"value":"{:exec (1 integer_add true exec_while (1 integer_add true)), :code nil, :integer (29), :float nil, :boolean ()}"}
;; <=

;; **
;;; This program pushes `0` on the `:integer` stack, then `true` on the `:boolean` stack, and then starts running a "while" loop that will keep executing as long as it finds `true` on top of the `:boolean` stack. Since the body of the loop itself pushes another `true` (along with adding `1` to the top integer), it will never stop.
;;; 
;;; But it did stop! And the top integer only got up to `29`. What's going on?
;;; 
;;; The answer is that it hit the step limit, and returned the state at that point.
;;; 
;;; The step limit is stored in an atom that we can query this way:
;; **

;; @@
@global-evalpush-limit
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>150</span>","value":"150"}
;; <=

;; **
;;; So currently it will just run for 150 steps. As for why that gets us only up to `29`, that has to do with the steps required for the other parts of the program and the mechanics of `exec_while`, which does its work by pushing stuff to the `:exec` stack and executing it.
;;; 
;;; We can change the step limit like this:
;; **

;; @@
(reset! global-evalpush-limit 1000)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>1000</span>","value":"1000"}
;; <=

;; **
;;; And then it'll get further before stopping:
;; **

;; @@
(stks (run-push '(0 true exec_while 
                  (1 integer_add true))
                (make-push-state)))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:exec</span>","value":":exec"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-symbol'>integer_add</span>","value":"integer_add"},{"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"},{"type":"html","content":"<span class='clj-symbol'>exec_while</span>","value":"exec_while"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-symbol'>integer_add</span>","value":"integer_add"},{"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"}],"value":"(1 integer_add true)"}],"value":"(1 integer_add true exec_while (1 integer_add true))"}],"value":"[:exec (1 integer_add true exec_while (1 integer_add true))]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:code</span>","value":":code"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:code nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:integer</span>","value":":integer"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>199</span>","value":"199"}],"value":"(199)"}],"value":"[:integer (199)]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:float</span>","value":":float"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:float nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:boolean</span>","value":":boolean"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"}],"value":"[:boolean ()]"}],"value":"{:exec (1 integer_add true exec_while (1 integer_add true)), :code nil, :integer (199), :float nil, :boolean ()}"}
;; <=

;; **
;;; Incidentally, you can tell whether a call to run-push terminated normally or by hitting the step limit by looking at the `:termination` of the returned Push state. We've been hiding this with our `stks` function, but here's that same call without `stks`, where you can see that the `:termination` is `:abnormal`:
;; **

;; @@
(run-push '(0 true exec_while (1 integer_add true))
          (make-push-state))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:exec</span>","value":":exec"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-symbol'>integer_add</span>","value":"integer_add"},{"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"},{"type":"html","content":"<span class='clj-symbol'>exec_while</span>","value":"exec_while"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-symbol'>integer_add</span>","value":"integer_add"},{"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"}],"value":"(1 integer_add true)"}],"value":"(1 integer_add true exec_while (1 integer_add true))"}],"value":"[:exec (1 integer_add true exec_while (1 integer_add true))]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:code</span>","value":":code"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:code nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:integer</span>","value":":integer"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>199</span>","value":"199"}],"value":"(199)"}],"value":"[:integer (199)]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:float</span>","value":":float"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:float nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:boolean</span>","value":":boolean"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"}],"value":"[:boolean ()]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:char</span>","value":":char"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:char nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:string</span>","value":":string"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:string nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:zip</span>","value":":zip"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:zip nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:vector_integer</span>","value":":vector_integer"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:vector_integer nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:vector_float</span>","value":":vector_float"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:vector_float nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:vector_boolean</span>","value":":vector_boolean"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:vector_boolean nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:vector_string</span>","value":":vector_string"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:vector_string nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:input</span>","value":":input"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:input nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:output</span>","value":":output"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:output nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:auxiliary</span>","value":":auxiliary"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:auxiliary nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:tag</span>","value":":tag"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:tag nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:return</span>","value":":return"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:return nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:environment</span>","value":":environment"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:environment nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:genome</span>","value":":genome"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:genome nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:termination</span>","value":":termination"},{"type":"html","content":"<span class='clj-keyword'>:abnormal</span>","value":":abnormal"}],"value":"[:termination :abnormal]"}],"value":"#clojush.pushstate.PushState{:exec (1 integer_add true exec_while (1 integer_add true)), :code nil, :integer (199), :float nil, :boolean (), :char nil, :string nil, :zip nil, :vector_integer nil, :vector_float nil, :vector_boolean nil, :vector_string nil, :input nil, :output nil, :auxiliary nil, :tag nil, :return nil, :environment nil, :genome nil, :termination :abnormal}"}
;; <=

;; **
;;; All of the Push programs we've seen so far have been completely self-contained in the sense that the data they process is encoded directly within them. How can you give a program *inputs*?
;;; 
;;; One way is to push them onto stacks before you run the program. The `push-item` utility function makes this easy. For example:
;; **

;; @@
(stks (run-push '(float_dup float_mult 
                  3.141592 float_mult)
                (push-item 2.5 
                           :float 
                           (make-push-state))))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:exec</span>","value":":exec"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[],"value":"()"}],"value":"[:exec ()]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:code</span>","value":":code"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:code nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:integer</span>","value":":integer"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:integer nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:float</span>","value":":float"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>19.63495</span>","value":"19.63495"}],"value":"(19.63495)"}],"value":"[:float (19.63495)]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:boolean</span>","value":":boolean"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:boolean nil]"}],"value":"{:exec (), :code nil, :integer nil, :float (19.63495), :boolean nil}"}
;; <=

;; **
;;; Here we pushed `2.5` onto the `:float` stack before we ran the Push program. The push program squared the initial value (by dulicating it and then multiplying the two copies), and then multiplied it by @@\pi@@. So it computed the area of a circle from a radius. You could call the same program on different initial states to compute different circle areas.
;;; 
;;; Sometimes it's even more convenient to have instructions that push inputs. That way the program can refer to them whenever it wants, without keeping track of where the initially-pushed items are on the stacks. 
;;; 
;;; This is the job of Clojush's "input instructions," which work with the `:input` stack. The idea is that you push the inputs onto the `:input` stack (rather than the `:float` or other stacks), and then instructions like `in1`, `in2`, etc. can be used to push those values.
;;; 
;;; Here's a simple example:
;; **

;; @@
(run-push '(in1 in1 float_mult 3.141592 float_mult)
          (push-item 2.5 :input (make-push-state)))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:exec</span>","value":":exec"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[],"value":"()"}],"value":"[:exec ()]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:code</span>","value":":code"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:code nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:integer</span>","value":":integer"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:integer nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:float</span>","value":":float"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>19.63495</span>","value":"19.63495"}],"value":"(19.63495)"}],"value":"[:float (19.63495)]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:boolean</span>","value":":boolean"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:boolean nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:char</span>","value":":char"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:char nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:string</span>","value":":string"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:string nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:zip</span>","value":":zip"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:zip nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:vector_integer</span>","value":":vector_integer"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:vector_integer nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:vector_float</span>","value":":vector_float"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:vector_float nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:vector_boolean</span>","value":":vector_boolean"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:vector_boolean nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:vector_string</span>","value":":vector_string"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:vector_string nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:input</span>","value":":input"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>2.5</span>","value":"2.5"}],"value":"(2.5)"}],"value":"[:input (2.5)]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:output</span>","value":":output"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:output nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:auxiliary</span>","value":":auxiliary"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:auxiliary nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:tag</span>","value":":tag"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:tag nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:return</span>","value":":return"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:return nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:environment</span>","value":":environment"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:environment nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:genome</span>","value":":genome"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:genome nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:termination</span>","value":":termination"},{"type":"html","content":"<span class='clj-keyword'>:normal</span>","value":":normal"}],"value":"[:termination :normal]"}],"value":"#clojush.pushstate.PushState{:exec (), :code nil, :integer nil, :float (19.63495), :boolean nil, :char nil, :string nil, :zip nil, :vector_integer nil, :vector_float nil, :vector_boolean nil, :vector_string nil, :input (2.5), :output nil, :auxiliary nil, :tag nil, :return nil, :environment nil, :genome nil, :termination :normal}"}
;; <=

;; **
;;; Here again I didn't use `stks` because I wanted to show the `:input` stack, so you can see that it contains one item, `2.5`. The `:input` stack is a stack like any other, but there aren't any instructions defined for it except the input instructions `in1`, `in2`, etc. So it never changes, aside from when you manually stick stuff on it before running your Push program.
;;; 
;;; There are a *lot* more instructions and features in Clojush, but this is enough to get see how things work in general, and it's time to move on to using the `pushgp` function to evolve Push programs.
;;; 
;;; The `pushgp` function takes a single argument which should be a map containing key/value pairs for any or all of the arguments listed in Clojush's [pushgp.clj](https://github.com/lspector/Clojush/blob/c6e9bc5e9835ed3c57703509e5b9fb988ea5f081/src/clojush/pushgp/pushgp.clj) file.
;;; 
;;; The only argument that is absolutely necessary is `:error-function` which should be a function that takes a Push program and returns a vector of errors (with lower being better and `0` being perfect). The reason it's a vector and not a single value is because you'll often want to run your program on a bunch of inputs, and this lets you return all of the individual test case errors, which might later be combined in various ways, depending on other options. If you have only one test case you should still have your error function return this in a vector.
;;; 
;;; In the example below I've specied an `:error-function` that evaluates the program for integer inputs from `0` to `9`, and for each input there's an error indicating how far the output is from @@x^3-2x^2-x@@, where the input is @@x@@. If there's nothing on the `:float` stack after the program runs (in which case `top-item` will return `:no-stack-item` rather than a number), then the error will be `1000000` (so, really bad).
;;; 
;;; The other arguments I've specified in the example below are `:atom-generators`, `:population-size`, `:parent-selection`, and `:use-single-thread`.
;;; 
;;; The `:atom-generators` argument specifies what's in the "primordial ooze"; that is, what ingredients can occur in evolving programs. By default this will be all known instructions, including some that generate random values, and you usually don't want all of those. So you'll usually want to provide your own value for this too.
;;; 
;;; For `:population-size` I supplied the value `100`, simply because the default of `1000` runs a little more slowly that I wanted for a tutorial.
;;; 
;;; For `:parent-selection` I supplied the value `:tournament`, which means that parents will be selected using tournament selection with the default tournament size (which happens to be 7).
;;; 
;;; Finally, I included `:use-single-thread true`. Why? By default, Clojush will run in a multi-threaded mode and use all available cores, which should make it run faster. But if you do something wrong, and cause an exception to be raised in one of the threads, Clojush will exit unceremoniously and without providing helpful feedback. (And if you're working in Gorilla REPL it will kill the server and you'll have to restart it, and you might have to jump through hoops to avoid losing unsaved changes to your worksheet.) The reasons for this are too boring to recount here, but the bottom line is that `:use-single-thread true` will prevent the exit, and you'll see normal error messages. I recommend running in this mode until you're confident that everything is working, and then run with `:use-single-thread false` (which is the default if you don't provide this argument at all) for better performance.
;;; 
;;; The following call to pushgp is the last thing in this tutorial. It's **long** because Clojush prints **lots** of info about the run as it proceeds, so that we can study how it is working and try to improve it. Many of the things printed won't make much sense without reading either more of the code base or publications linked to http://pushlanguage.org. But you can see the final result of the run near the bottom of the file.
;;; 
;; **

;; @@
(pushgp 
  {:error-function 
   (fn [{:keys [program] :as individual}]
     (assoc individual
       :errors
       (vec
         (for [input (mapv float (range 10))]
           (let [output (->> (make-push-state)
                             (push-item input :input)
                             (run-push program)
                             (top-item :float))]
             (if (number? output)
               (Math/abs (float (- output 
                                   (- (* input 
                                         input 
                                         input) 
                                      (* 2 input input)
                                      input))))
               1000000))))))
   :atom-generators (list 'in1
                          'float_div
                          'float_mult
                          'float_add
                          'float_sub)
   :population-size 100
   :parent-selection :tournament
   :use-single-thread true})
;; @@

;; @@

;; @@
