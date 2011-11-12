README.txt

Lee Spector (lspector@hampshire.edu), started 20100227
See version history at https://github.com/lspector/Clojush/commits/master

This is the README file accompanying Clojush, an implementation of the 
Push programming language and the PushGP genetic programming system in the 
Clojure programming language. Among other features this implementation
takes advantage of Clojure's facilities for multi-core concurrency. Use
Java's -XX:+UseParallelGC option to take maximum advantage of this feature.

AVAILABILITY

https://github.com/lspector/Clojush/

REQUIREMENTS

To use this code you must have a Clojure programming environment; see 
http://clojure.org/. The current version of clojush requires clojure 1.2
and clojure-contrib 1.2.

Clojure is available for most OS platforms. Good starting points for
obtaining and using Clojure include http://clojure.org/getting_started
and http://www.assembla.com/wiki/show/clojure/Getting_Started.

QUICKSTART

To run the system on a simple example start a clojure REPL (read, evaluate,
print loop) in the directory that contains clojush.clj and the examples 
subdirectory and type the following to the REPL prompt: 

    (load "examples/simple-regression")

This will load everything and run PushGP on a simple symbolic 
regression problem (symbolic regression of y=x^3-2x^2-x). Although the 
details will vary from run to run, and it's possible that it will fail, 
this usually succeeds in a few generations. 

If you have Leiningen (from https://github.com/technomancy/leiningen)
then you can run an example from the OS command line with a call like:

    lein run examples.simple-regression

In some IDEs there will be other ways to run the examples. For example,
in Eclipse/Counterclockwise you can simply open an example and select
Clojure > Load clojure file in REPL.

You may want to provide additional arguments to java in order to allow 
access to more memory and/or to take maximal advantage of Clojure's
concurrency support in the context of clojush's reliance on garbage 
collection. For example, you might want to provide arguments such 
as -Xmx2000m and -XX:+UseParallelGC.

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
          (concat @registered-instructions     ;; all registered instrs 
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
  evalpush-time-limit) that may save you from exponential code growth or other
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

VERSION HISTORY -- NOW OBSELETE. See https://github.com/lspector/Clojush/commits/master
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
          - Added atom for registered-instructions; NOTE: requires user
            code that refers to registered-instructions to refer to 
            @registered-instructions instead. (Example odd.clj changed
            to reflect this.)
          - Added to-do item "Convert structs to records, which should be
			faster. Experiments with Clojure 1.2 show this to be faster
            but there are not good examples yet to serve as the basis for 
            changes.
          - Added atoms for global-atom-generators and 
            global-max-points-in-program.
          - Changed pushgp to take keyword arguments rather than a parameter
            map. NOTE: this requires calls to pushgp to be written differently.
            Updated examples to reflect this.
20100921: - Removed random-element in favor of rand-nth.
          - Cleaned up indentation, miscellaneous other cosmetic things.
          - Added namespaces for all example files.
          - Updated README to mention requirement for clojure 1.2 and to
            remove mention of ClojureX which has been discontinued.
          - Converted structures to records (a clojure 1.2 feature, should
            be faster).
20101005: - Added error-handlers to agents.
20101014: - [Artificial ant, krypto, tg8, decimation]
          - Added articial ant example from Koza (via Luke).
          - Added "tg8" integer symbolic regression problem.
          - Added krypto number puzzle example.
          - Added pushgp "decimation" feature, in which elimination 
            tournaments, conducted after fitness testing, reduce the
            size of the population to a specified fraction of its original
            size (specified in a decimation-ratio argument to pushgp;
            the tournament sized is specified via decimation-tournament-size).
            The ordinary tournament-size parameter is still used for subsequent
            selection from the decimated population. Any specified trivial
            geography applies both to decimation and to subsequent selection.
20101017: - Reverted from records to structs; wasn't significantly faster and
            structs allow for greater flexibility in use of state as map.
20101102: - Switched to new clojure.core/frequencies from depricated 
            seq-utils/frequencies (h/t Kyle Harrington), and similarly
            for flatten.
          - Added :include-randoms keyword argument for registered-for-type.
            Defaults to true, but if specified as false then instructions 
            ending with "_rand" will be excluded.
          - Raised invalid output penalty in tg8 (it was lower than reasonable
            errors for that problem).
20101103: - Converted evalpush-limit and evalpush-time-limit into vars 
 			(global-evalupush-limit and global-evalpush-time-limit) bound
            to atoms that are reset by calls to pushgp (keyword arguments
            :evalpush-limit and :evalpush-time-limit).
          - Changed pushgp parameters in the tg8.clj example.
20101104: - Implemented stackless tagging (see http://push.i3ci.hampshire.edu/
            2010/10/28/stackless-tagging/). Tag instructions take one of
            the following forms:
              tag_<type>_<number> 
                create tage/value association, with the value taken from the 
                stack of the given type and the number serving as the tag
              untag_<number>
                remove the association for the closest-matching tag
              tagged_<number> 
                push the value associated with the closest-matching tag onto 
                the exec stack (or no-op if no associations).
            Here "closest-matching" means the tag with lowest number that 
            is greater than or equal to the number of the tag being matched,
            or the lowest-numbered tag if none meet this criterion. Tag
            instructions are not implemented in the same way as other 
            instructions; they are detected and handled specially by the
            interpreter (see execute-instruction). Tag instructions
            can be included in pushgp runs by using the new ephemeral
            random constant functions tag-instruction-erc, 
            untag-instruction-erc, and tagged-instruction-erc, each of
            which takes a limit (for the numerical part) as an 
            argument.
          - Added examples using tags: tagged_ant, tagged_regression, and
            tagged_tg8.
20101106: - Tweaked parameters in ant examples; among other things,
            increased simplification since bloat was an issue. Also
            added some evolved solutions in comments.     
20101107: - Added Koza's lawnmower problem example; this demonstrates how
            to add a new type/stack on a problem-specific basis, without
            altering clojush.clj.    
20101204: - Added pushgp-map, which allows pushgp calls on maps of arguments,
            and a demonstration of its use in argmap_regression.clj.
          - Added :gen-class and -main definition (thanks Kyle Harrington).
          - Fixed eval-push termination to return :abnormal for exceeding
            time-limit (thanks Kyle Harrington).
20101205: - Added modified version of Kyle's version of the intertwined
            spirals problem.
          - Minor changes to this README. 
20101208: - Added alternative methods for node selection, used in mutation
            and crossover (drafted by Brian Martin, with suggestions
            from Kyle Harrington). This introduced three new globals:
            global-node-selection-method, global-node-selection-leaf-probability,
            and global-node-selection-tournament-size, each of which holds
            an atom, and three new parameters to pushgp: node-selection-method,
            node-selection-leaf-probability, and node-selection-tournament-size.
            The node-selection-method can be :unbiased (in which case nodes
            are selected using the uniform distribution that was previously
            used -- this is the default), :leaf-probability (in which case
            the value of the node-selection-leaf-probability argument,
            which defaults to 0.1, specifies the probability that leaves,
            as opposed to internal nodes, will be selected -- this is the
            method used by Koza and others in tree-based GP), or
            :size-tournament (in which case the value of the 
            node-selection-tournament-size argument, which defaults to 2,
            determines the tournament size for node tournaments, with the
            largest subtree in the tournament set being selected).
20110111: - Added zipper stack and functions (thanks to Kyle Harrington for
            draft code, although this was re-written).
          - Added registered-nonrandom function.
          - Modified odd.clj example to use registered-nonrandom.
          - Added examples/dsoar.clj, a version of the "Dirt-Sensing,
            Obstacle-Avoiding Robot" (DSOAR) problem first described in:
              Spector, L. 1996. Simultaneous Evolution of Programs and their
              Control Structures. In Advances in Genetic Programming 2, edited
              by P. Angeline and K. Kinnear, pp. 137-154. Cambridge, MA: MIT Press.
              http://helios.hampshire.edu/lspector/pubs/AiGP2-post-final-e.pdf
            This version was written by Brian Martin in 2010-2011.
20110118: - Added support for tagged_code_<number> instructions. These are like
            tagged_<number> instructions except that retrieved values are pushed
            onto the code stack rather than the exec stack. Without these the
            only way to get tagged values onto the code stack is to wrap
            values with code_quote prior to tagging. An alternative approach
            is to add new tagging instructions that automatically perform
            code_quote wrapping, but for full generality that would require
            new instructions for each type; also quote-tagged values would 
            always be destined for the code stack, while the scheme adopted
            here allows any stored value to be retrieved either to exec or to
            code.
          - A value of 0 for the evalpush-time-limit parameter of pushgp
            now means that no time limit will be enforced. This is also
            now the default.
20110322: - Tag reference bug fixed in closest-association (>= changed to <=).
          - Added mux (multiplexer) example (a couple of approaches in one file).
20110409: - Added support for no-pop tagging through a var called
            global-pop-when-tagging (holding an atom with a boolean value)
            and a boolean argument to pushgp called pop-when-tagging. 
            The defaults are true, for backwards compatibility. If 
            @global-pop-when-tagging is false (which will result from 
            passing false as a :pop-when-tagging keyword argument to pushgp)
            then executing instructions of the form tag_<type>_<number>
            will tag a value as usual, but the tagged item will not be popped
            from its source stack.
          - Removed no-pop hackage from mux example (thanks Kyle).
20110424: - Added Gaussian mutation for floating point literals. This is 
            a genetic operator on par with ordinary mutation, crossover,
            and simplification, with the probability for applying this operator
            set with the gaussian-mutation-probability argument to pushgp
            (which defaults to zero). The behavior of this operator, when used,
            is controlled by two other arguments to pushgp, 
            gaussian-mutation-per-number-mutation-probability (which is the
            probability that any particular floating point number in the 
            program will actually be mutated -- this defaults to 0.5) and
            gaussian-mutation-standard-deviation (which specifies the standard
            deviation of the Gaussian noise generator that is used to 
            produce changes to numbers -- this defaults to 0.1).
          - Added examples/gaussian_mutation_demo.clj to demonstrate Gaussian
            mutation.
          - Added examples/korns_regression_p12.clj, a symbolic regression
            problem based on Michael Korns's draft chapter from GPTP11.
20110505: - Added complex number support.  New instructions for the 'complex' 
            stack include: pop, dup, swap, rot, flush, eq, stackdepth, yank, 
            yankdup, shove, rand, add, sub, mult, divide, fromfloat, 
            frominteger, fromfloats, fromintegers, conjugate, magnitude, 
            and principal_sqrt. (Brian Martin)
20110517: - Added a "scaled-errors" function to support error-scaling as 
            described by Maarten Keijzer in Scaled Symbolic Regression, in
            Genetic Programming and Evolvable Machines 5(3), pp. 259-269, 
            September 2004. This must be used in a problem's error function,
            and then the outputs of the evolved program must be "unscaled."
            See the documentation string for scaled-errors and also
            examples/scaled_sextic.clj for details.
          - Added examples/scaled_sextic.clj to demonstrate the use of
            scaled-errors.
          - Changed examples/sextic.clj to use squared errors and an error
            threshold, in order to facilitate comparisons between the
            versions that do and don't use error scaling.
          - Made minor changes to the korns_regression_p12 example.
20110526: - Enforce size limits on zipper manipulation results.
20110607: - Added overlap utility function, overlap-demo (which just prints
            some examples to show how overlap works), and code_overlap
            instruction. Overlap can be applied to any two (possibly
            nested) things and it returns a number between 0 (meaning
            no overlap) and 1 (meaning exact match). The overlap utility
            function returns a ratio, but the code_overlap instruction
            pushes a float.
          - Removed complex number support from 20110505. There were previous
            reports of problems and I've noticed problems from the fact that
            (apply + ()) => zero (as opposed to 0) in the clojush namespace
            defined by the code as revised for complex number support. If
            someone knows how to re-introduce complex number support without
            such problems then please let me know.
20110618: - Switched to Kyle Harrington's version of overlap; it's more clear,
            possibly faster, and may fix a hard-to-trace bug that cropped up
            in a long evolutionary run (?).
20110624: - Replaced lawnmower and dsoar examples with bugfixed versions
            (thanks to Kyle Harrington).
          - Added namespace and made miscellaneous other changes to 
            clojush-tests.clj.
          - Added support for tagged-code macros. Tagged-code macro calls
            have the effect of code instructions, but they take their
            code arguments from the tag space and they tag their code return
            values. They are implemented as macros to leverage the existing
            code instruction set; note that this means that a single call
            will contribute more than one iteration step toward the 
            evalpush-limit. Tagged-code macros appear in programs as hash maps
            that map :tagged_code_macro to true, :instruction to a code
            instruction, :argument_tags to a sequence of tags to be used
            to acquire arguments, and :result_tags to a sequence of tags
            to be used for tagging results. Execution of a macro expands
            the needed code onto the exec stack to grab arguments from the tag 
            space and push them on the code stack, execute the code instruction, 
            and tag results. Note that results will also be left on the code
            stack if global-pop-when-tagging is false. Conceptually, tag values
            are "baked in" to these macros in much the same way that tag values
            are "baked in" to the instruction names for stackless tag
            instructions; we use hash maps simply because there is more
            information to bake in and this prevents us from having to parse
            the names (which would get messy and also waste time). Because
            the maps make it difficult to read programs, however, a utility
            function called abbreviate-tagged-code-macros is provided to
            produce copies of programs with more human-readable (but not 
            currently executable) representations of tagged-macro calls.
            A tagged-code-macro-erc is provided to generate random tagged-code
            macros in pushgp runs. A new example, codesize20, provides 
            a simple demonstration of the use of tagged-code macros.
          - Replaced walk-based code-manipulation with walklist functions
            that only traverse list structure. This fixes an interaction
            between map literals (e.g. tagged-code macros) and program
            structure.
20110629: - Fixed abbreviate-tagged-code-macros printing of empty lists.
          - Added seq condition to walklist to permit walking of seqs that
            aren't actually full-fledged lists.
20110702: - Several fixes/refinements to tagged-code macros: 
            - Fixed incorrect no-op of arg-free calls with empty tag space.
            - Added :additional_args to tagged-code macro structure; the
              value should be a list of items and these will be executed
              in order before calling the macro's instruction.
            - Added optional 5th arg to tagged-code-macro-erc; this should
              be a function of zero args that will be called to produce
              the value of :additional_args (e.g. if you want to have one
              random integer arg then you could specify a 5th arg of
              (fn [] (list (lrand-int 101))).
            - Changed format produced by abbreviate-tagged-code-macros to
              handle :additional_args and to be slightly more concise.
20110714: - Added "trace" argument to eval-push and run-push. If this is
            true then the resulting state will map :trace to a list of
            executed instructions and literals, in reverse order of
            execution. If the argument is :changes then instructions that
            have no effect on the state will be excluded.
20110809: - Several additions/enhancements by Kyle Harrington:
            - Converted problem-specific-report to a parameter in pushgp.
            - Added reporting of program repeat counts in population.
            - Added "error-reuse" parameter to pushgp for use in stochastic
              and dynamic problems (for which reuse would be turned off).
            - Added examples/mackey_glass_int.clj, a symbolic regression 
              problem as described in Langdon & Banzhaf's 2005 paper 
              (citation in file).
            - Added examples/pagie_hogeweg.clj problem, a difficult 
              symbolic regression problem when coevolution is not used.
              Introduced by Pagie & Hogeweg's 1997 paper (citation in file).
20110911: - Switched from Eclipse to Clooj/Leiningen for development and  
            rearranged the project for this.
          - Added calls to end of all example files.
          - Examples can be run from the OS command line (assuming 
            that leiningen is available) with calls like:
              lein run examples.simple-regression
          - Added local-file dependency and used file* for file access.
          - Removed ant and tagged-ant examples because of bugs related
            to confusion of push interpreted states and ant world states.
20111104: - Added string stack and a variety of string stack instructions.
          - Added two example pushgp runs that use the string stack in
            the file examples/string.clj.
20111112: - Obsoleted this version history in favor of the commit logs
            at https://github.com/lspector/Clojush/commits/master
          

ACKNOWLEDGEMENTS

This material is based upon work supported by the National Science Foundation 
under Grant No. 1017817. Any opinions, findings, and conclusions or 
recommendations expressed in this publication are those of the authors and 
do not necessarily reflect the views of the National Science Foundation.
