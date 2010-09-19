README.txt

Lee Spector (lspector@hampshire.edu), started 20100227
See version history at the end of this file.

This is the README file accompanying clojush.clj, an implementation of the 
Push programming language and the PushGP genetic programming system in the 
Clojure programming language. Among other features this implementation
takes advantage of Clojure's facilities for multi-core concurrency. Use
Java's -XX:+UseParallelGC option to take maximum advantage of this feature.

REQUIREMENTS

To use this code you must have a clojure programming environment; see 
http://clojure.org/.

Clojure is available for most OS platforms. If you are using Mac OS X
then you might want to check out http://github.com/citizen428/ClojureX.

QUICKSTART

To run the system on a simple example start a clojure REPL (read, evaluate,
printloop) in the directory that contains clojush.clj and the examples 
subdirectory and type the following to the REPL prompt: 

    (load "examples/simple-regression")

This will load everything and run PushGP on a simple symbolic 
regression problem (symbolic regression of y=x^3-2x^2-x). Although the 
details will vary from run to run, and it's possible that it will fail, 
this usually succeeds in a few generations. 

Details of starting the clojure REPL will depend on your programming 
environment, but if you're using Mac OS X and the installation mentioned 
above then you would use the Terminal application, use "cd" to get to the 
right directory, and then type "clj" to start the REPL. You can quit the 
REPL with control-D.

If your Clojure installation provides a clj command-line script (as does
ClojureX) then you can run an example from the OS command line, without
starting a REPL, by using a command like:

    clj -i examples/simple-regression.clj

You may want to provide additional arguments to clj (or to java if you
are invoking it without a clj script) in order to allow access to more
memory and/or to take maximal advantage of Clojure's concurrency support
in the context of Clojush's reliance on garbage collection. For example,
you might want to issue your call as:

    clj -Xmx2000m -XX:+UseParallelGC -i examples/simple-regression.clj



DESCRIPTION

Clojush is a version of the Push programming language for evolutionary 
computation, and the PushGP genetic programming system, implemented in 
clojure. More information about Push and PushGP can be found at 
http://hampshire.edu/lspector/push.html.

Clojush derives mainly from Push3 (for more information see
http://hampshire.edu/lspector/push3-description.html,
http://hampshire.edu/lspector/pubs/push3-gecco2005.pdf) but it is not 
intended to be fully compliant with the Push3 standard and there are a 
few intentional differences. It was derived most directly from the Scheme
implementation of Push/PushGP (called Schush). There are several differences
between Clojush and other versions of Push3 -- for example, almost all of the
instruction names are different because the "." character has special
significance in Clojure -- and these are listed below.

If you want to understand the motivations for the development of Push, and 
the variety of things that it can be used for, you should read a selection of
the documents listed at http://hampshire.edu/lspector/push.html, probably
starting with the 2002 Genetic Programming and Evolvable Machines article
that can be found at http://hampshire.edu/lspector/pubs/push-gpem-final.pdf.
But bear in mind that Push has changed over the years, and that Clojush is
closest to Push3 (references above).

Push can be used as the foundation of many evolutionary algorithms, 
not only PushGP (which is more or less a standard GP system except
that it evolves Push programs rather than Lisp-style function trees --
which can make a big difference!). It was developed primarily for
"meta-genetic-programming" or "autoconstructive evolution" experiments, in
which programs and genetic operators co-evolve or in which programs produce
their own offspring while also solving problems. But it turns out that
Push has a variety of uniquely nice features even within a more
traditional genetic programming context; for example it makes it unusually
easy to evolve programs that use multiple data types, it provides novel and
automatic forms of program modularization and control structure co-evolution,
and it allows for a particularly simple form of automatic program
simplification. Clojush can serve as the foundation for other evolutionary 
algorithms, but only the core Push interpreter and a version of PushGP
are provided here.

USAGE

Loading clojush.clj as distributed should load everything and print the list 
of registered Push instructions. Example calls to PushGP are provided in other
accompanying files.

Push programs are run calling run-push, which takes as arguments a Push 
program and a Push interpreter state that can be made with make-push-state. 
If you are planning to use PushGP then you will want to use this in the error 
function (a.k.a. fitness function) that you pass to the pushgp function. 
Here is a simple example of a call to run-push, adding 1 and 2 and returning 
the top of the integer stack in the resulting interpreter state:

(top-item :integer (run-push '(1 2 integer_add) (make-push-state)))

To try this paste it below all of the code in clojush.clj and load the file
in the Clojure REPL, or alternatively load clojush.clj as-is, then tell the
REPL (in-ns 'clojush), and then type the above expression directly to the
REPL prompt.

If you want to see every step of execution you can pass an optional third
argument of true to run-push. This will cause a representation of the
interpreter state to be printed at the start of execution and after
each step. Here is the same example as above but with each step printed:

(top-item :integer (run-push '(1 2 integer_add) (make-push-state) true))

See the "parameters" section of the code for some parameters that will affect 
execution, e.g. whether code is pushed onto and/or popped off of the code
stack prior to/after execution, along with the evaluation limits (which can be
necessary for halting otherwise-infinite loops, etc.).

Run-push returns the Push state that results from the program execution; this
is a Clojure map mapping type names to data stacks. In addition, the map 
returned from run-push will map :termination to :normal if termination was 
normal, or :abnormal otherwise (which generally means that execution was
aborted because the evaluation limit was reached.

Random code can be generated with random-code, which takes a size limit and a 
list of "atom generators." Size is calculated in "points" -- each atom and
each pair of parentheses counts as a single point. Each atom-generator should
be a constant, or the name of a Push instruction (in which case it will be
used literally), or a Clojure function that will be called with no arguments
to produce a constant or a Push instruction. This is how "ephemeral random
constants" can be incorporated into evolutionary systems that use Clojush --
that is, it is how you can cause random constants to appear in
randomly-generated programs without including all possible constants in the
list of elements out of which programs can be constructed. Here is an example
in which a random program is generated, printed, and run. It prints a message
indicating whether or not the program terminated normally (which it may not,
since it may be a large and/or looping program, and since the default
evaluation limit is pretty low) and it returns the internal representation of
the resulting interpreter state:

(let [s (make-push-state)
      c (random-code 
          100                                  ;; size limit of 100 points
          (concat registered-instructions      ;; all registered instrs 
                  (list (fn [] (rand-int 100)) ;;  random integers from 0-99
                        (fn [] (rand)))))]     ;; random floats from 0.0-1.0
  (printf "\n\nCode: %s\n\n" (apply list c))
  (run-push c s))

If you look at the resulting interpreter state you will see an "auxiliary" 
stack that is not mentioned in any of the Push publications. This exists 
to allow for auxiliary information to be passed to programs without using 
global variables; in particular, it is used for the "input instructions"
in some PushGP examples. One often passes data to a Push program by pushing 
it onto the appropriate stacks before running the program, but in many 
cases it can also be helpful to have an instruction that re-pushes the 
input whenever it is needed. The auxiliary stack is just a convenient place 
to store the values so that they can be grabbed by input instructions and 
pushed onto the appropriate stacks when needed. Perhaps you will find other 
uses for it as well, but no instructions are provided for the auxiliary stack 
in Clojush (aside from the problem-specific input functions in the examples).

The pushgp function is used to run PushGP. It takes all of its arguments
in a Clojure map, and provides default values for any parameters that are
not provided. Search clojush.clj for "defn pushgp" for details. The single
argument that must be provided (actually it too has a default, but it makes
no sense to rely on that) is :error-function, which should be a function that
takes a Push program and returns a list of errors. Note that this assumes 
that you will be doing single-objective evolution with the objective being 
thought of as an error to be minimized. This assumption not intrinsic to Push
or PushGP; it's just the simplest and most standard thing to do, so
it's what I've done here. One could easily hack around that. In the most
generic applications you'll want to have your error function run through a
list of inputs, set up the interpreter and call run-push for each,
calculate an error for each (potentially with penalties for abnormal
termination, etc.), and return a list of the errors.

Not all of the default arguments to pushgp will be reasonable for all 
problems. In particular, the default list of atom-generators -- which is ALL
registered instructions, a random integer generator (in the range from 0-99)
and a random float generator (in the range from 0.0 to 1.0) -- will be
overkill for many problems and is so large that it may make otherwise simple
problems quite difficult because the chances of getting the few needed
instructions together into the same program will be quite low. But on the
other hand one sometimes discovers that interesting solutions can be formed
using unexpected instructions (see the Push publications for some examples of
this). So the set of atom generators is something you'll probably want to
play with. The registered-for-type function can make it simpler to include
or exclude groups of instructions. This is demonstrated in some of the 
examples.

Other pushgp arguments to note include those that control genetic 
operators (mutation, crossover, and simplification). The specified operator
probabilities should sum to 1.0 or less -- any difference between the sum and
1.0 will be the probability for "straight" (unmodified) reproduction. The use
of simplification is also novel here. Push programs can be automatically 
simplified -- to some extent -- in a very straightforward way: because 
there are almost no syntax constraints you can remove anything (one or more 
atoms or sub-lists, or a pair of parentheses) and still have a valid
program. So the automatic simplification procedure just iteratively removes
something, checks to see what that does to the error, and keeps the simpler
program if the error is the same (or lower!).

Automatic simplification is used in this implementation of PushGP in three 
places: 

1. There is a genetic operator that adds the simplified program to the next
generation's population. The use of the simplification genetic operator will
tend to keep programs smaller, but whether this has benificial or detrimental
effects on search performance is a subject for future research.

2. A specified number of simplification iterations is performed on the best 
program in each generation. This is produced only for the sake of the report, 
and the result is not added to the population. It is possible that the 
simplified program that is displayed will actually be better than the best 
program in the population. Note also that the other data in the report 
concerning the "best" program refers to the unsimplified program.

3. Simplification is also performed on solutions at the ends of runs. 

Note that the automatic simplification procedure will not always find all
possible simplifications even if you run it for a large number of iterations,
but in practice it does often seem to eliminate a lot of useless code (and to
make it easier to perform further simplification by hand).

If you've read this far then the best way to go further is probably to read
and run the examples.

IMPLEMENTATION NOTES

A Push interpreter state is represented here as a Clojure map that maps type 
names (keywords) to stacks (lists, with the top items listed first).

Push instructions are names of Clojure functions that take a Push 
interpreter state as an argument and return it modified appropriately. The
define-registered macro is used to establish the definitions and
also to record the instructions in the global list registered-instructions.
Most instructions that work the same way for more than one type are
implemented using a higher-order function that takes a type and returns a 
function that takes an interpreter state and modifies it appropriately. 
For example there's a function called popper that takes a type and returns 
a function -- that function takes a state and pops the right stack in the 
state. This allows us to define integer_pop with a simple form:

(define-registered integer_pop (popper :integer))

In many versions of Push RUNPUSH takes initialization code or initial stack 
contents, along with a variety of other parameters. The implementation of
run-schush here takes only the code to be run and the state to modify. Other
parameters are set globally in the parameters section below. At some point
some of these may be turned into arguments to run-push so that they aren't
global.

Miscellaneous differences between clojush and Push3 as described in the Push3
specification: 

- Clojush instruction names use "_" instead of "." since the latter has
  special meaning when used in Clojure symbols.
- Equality instructions use "eq" rather than "=" since the latter in not 
  explicitly allowed in clojure symbols.
- for similar reasons +, -, *, /, %, <, and > are replaced with  add, sub, 
  mult, div, mod, lt, and gt.
- Boolean literals are true and false (instead of TRUE and FALSE in 
  the Push3 spec). The original design decision was based on the fact that 
  Common Lisp's native Boolean literals couldn't used without conflating 
  false and the empty list (both NIL in Common Lisp).
- Clojush adds exec_noop (same as code_noop).
- Clojush includes an execution time limit (via the parameter 
  evalpush-time-limit that may save you from exponential code growth or other
  hazards. But don't forget to increase it if you expect legitimate programs
  to take a long time.

Push3 stuff not (yet) implemented:
- NAME type/stack/instructions
- Other missing instructions: *.DEFINE, CODE.DEFINITION, CODE.INSTRUCTIONS
- The configuration code and configuration files described in the Push3
  spec have not been implemented here. The approach here is quite different,
  so this may never be implemented

TO DO (SOMETIME, MAYBE)

- Clean up issues involving Push globals and PushGP parameters for the same
  or similar things.
- Implement remaining instructions in the Push3 specification.
- Add more examples.
- Add support for seeding the random number generator.
- Add improved genetic operators, e.g. fair mutation/crossover and
  constant-perturbing mutations.
- Improve the automatic simplification algorithm.
- Possibly rename the auxiliary stack the "input" stack if no other
  uses are developed for it.
- Write a "sufficient-args" fn/macro to clean up Push instruction definitions.

VERSION HISTORY
20100227: - First distributed version.
20100301: - Added (shutdown-agents) for proper termination.
20100306: - Added history (of total errors of ancestors) to individuals.
	  - Commented out (shutdown-agents) because it prevents multiple
            runs in a single launch of a Clojure REPL.
20100307: - Fixed bug in history: reproductive auto-simplification added
            was adding duplicate items.
20100314: - Added instructions: *_shove, code_extract, code_insert, 
  	    code_subst, code_contains, code_container, code_position,
	    code_discrepancy, *_rand. NOTE that the presence of *_rand
            instructions means that programs produced using the full set
            of instructions may be non-deterministic. As of this writing
            pushgp (via evaluate-individual) will evaluate an individual
            only once, so it will always have whatever fitness value it 
            had upon first testing.
	  - Added globals to support integer_rand and float_rand:
            min-random-integer, max-random-integer, min-random-float
            max-random-float.
          - Fixed bug in code_car that could produce nil.
          - Made execute-instruction safe for nil (although it shouldn't 
            arise anyway).
	  - Added stress-test for testing and debugging new Push
            instructions. See the stress-test documentation string for
            details.
          - Implemented size limits on intermediate results on code stack 
            (in code_cons, code_list, code-append, code_insert, code_subst, 
            exec_s, exec_y).
	  - Fixed bug in exec_s (was always a noop previously).
20100319: - Added problem-specific-report function (to be customized per
            problem). This can also be a convenient place to put other
            stuff that you want done once per generation.
          - Added support for saving lists of ancestors (maternal line
            only) along with global parameters to turn both this and the
            saving of total-error histories on and off.
          - Added missing calls to "keep-number-reasonable" in numeric
  	    Push instructions. This eliminates some potential crashes from
            runaway number growth.
20100320: - Added print-ancestors-of-solution parameter and code.
          - Print simplification in report only with non-zero value for
            report-simplifications parameter.
          - Added sextic polynomial regression example. This example also
            demonstrates the use of fitness penalties for abnormally
            terminating programs.
	  - Added a new argument to problem-specific-report (NOTE: not
            backward compatible).
20100417: - Added thread-local random number generator objects to avoid
            contention.
          - Print parameters at the start of pushgp.
          - Added readme comments about concurrency, -Xmx2000m, and
            -XX:+UseParallelGC.
          - Converted time limit code to use System/nanoTime (thanks to
            Brian Martin). This means that time limits must now be
            expressed in nanoseconds rather than milliseconds, and I
            believe it will eliminate contention for shared Date objects
            (but this should be checked; if there is contention then 
            we should revert to using Date and use thread-local date
            objects as is being done with the random number generator
            objects).
          - Added print-return utility function for debugging.
          - Added a new Push instruction, code_wrap, which pushes a 1-item
            list containing the previous top item of the code stack.
          - Added a new Push instruction, code_map, which acts much like
            Lisp's (or Scheme's or Clojure's) "map" functions, using the
            top item on the exec stack as the function to map and the top
            item on the code stack as the list to map it down. The list of
            results is left on top of the code stack. This is implemented
            as a "macro" instruction that expands into a Push code
            fragment that: 1) for each item in the list on top of the
            code stack (or for the single non-list item that is there)
            quotes the item onto the code stack and then runs the code
            that's on top of the exec stack; 2) uses code_wrap to push a
            list containing the last result onto the code stack; 3)
            executes as many instances of code_cons as are necessary to
            add all of the other results onto the list. Note that this
            will act like an ordinary "map" function only when the code on
            the exec stack leaves a single output on the code stack in
            place of each input on the code stack; if it consumes or
            produces more or less code then the effect will be quite
            different.
20100502: - Made thread-local random integer function (lrand-int) safe for
            bignums, but arguments greater than 2^31-1 are treated as if
            they were 2^31-1 (2147483647).
20100526: - Reimplemented subst to use clojure.walk/postwalk-replace. Also
            fixed comment, which described args backwards. (The behavior
            was correct, emulating Common Lisp subst.)
20100918: - Created Eclipse project.
          - Deleted re-load/backtrace utils.
          - Removed shuffle, as it is now in clojure.core (in Clojure 1.2).
          - Removed gratuitous def in define-registered.
          - Added atom for instruction-table.
          