Clojush [![Travis branch](https://img.shields.io/travis/lspector/Clojush/master.svg?style=flat-square)](https://travis-ci.org/lspector/Clojush) [![Coveralls branch](https://img.shields.io/coveralls/lspector/Clojush/master.svg?style=flat-square)](https://coveralls.io/github/lspector/Clojush) [![API Docs](https://img.shields.io/badge/api%20docs-master-blue.svg?style=flat-square)](http://lspector.github.io/Clojush/) [![Clojars Project](https://img.shields.io/clojars/v/clojush.svg?style=flat-square)](http://clojars.org/clojush)
=======



Lee Spector (lspector@hampshire.edu), started 20100227
[See version history](https://github.com/lspector/Clojush/commits/master).
Older version history is in `old-version-history.txt`.

This is the README file accompanying Clojush, an implementation of the
Push programming language and the PushGP genetic programming system in the
Clojure programming language. Among other features this implementation
takes advantage of Clojure's facilities for multi-core concurrency.

Availability
------------

https://github.com/lspector/Clojush/

Requirements
------------

To use this code you must have a Clojure programming environment; see
http://clojure.org/. The current version of Clojush requires Clojure 1.7.0.

Clojure is available for most OS platforms. [A good starting point for
obtaining and using Clojure](http://dev.clojure.org/display/doc/Getting+Started).


Quickstart
----------

Using [Leiningen](https://github.com/technomancy/leiningen) you can
run an example from the OS command line (in the Clojush directory) with
a call like:

    lein run clojush.problems.demos.simple-regression

If you would like to change a parameter, you may do so at the command line.
For example, to change the default population size from 1000 to 50, call:

    lein run clojush.problems.demos.simple-regression :population-size 50

Additional parameters may also be specified. All valid parameters with their descriptions can be found in `src/clojush/pushgp/pushgp.clj`.

The above calls will load everything and run PushGP on a simple symbolic
regression problem (symbolic regression of y=x^3-2x^2-x). Although the
details will vary from run to run, and it's possible that it will fail,
this usually succeeds in a few generations.

Another option is to evaluate in the leinigen REPL (Read Eval Print Loop):

    sh> lein repl
    ...
    clojush.core=> (use 'clojush.problems.demos.simple-regression)
    ...
    clojush.core=> (pushgp argmap)

Arguments to pushgp are specified in the `argmap` variable in the problem's
namespace.

To run the examples in an IDE (Integrated Development Environment) for
Clojure such as Clooj or Eclipse/Counterclockwise, load one of the
files in src/clojush/problems into the IDE's REPL, type `(pushgp argmap)`
into the REPL's input area, and hit the enter key.

You can also use [Docker](https://docs.docker.com/) to run examples, if you
don't want to install Clojure on your machine directly.

```bash
# first build the image. This needs to be re-done if any of the code changes
docker build -t lspector/clojush .
# then run it on a specific problem
docker run --rm lspector/clojush lein run clojush.problems.demos.simple-regression
```

For large-scale runs you may want to provide additional arguments to
Java in order to allow  access to more memory and/or to take maximal
advantage of Clojure's concurrency support in the context of Clojush's
reliance on garbage  collection. For example, you might want to provide
arguments such  as `-Xmx2000m` and `-XX:+UseParallelGC`. Details will depend
on the method that you use to launch your code.

An additional tutorial is available in `src/clojush/problems/demos/tutorial.clj`.

Description
-----------

Clojush is a version of the Push programming language for evolutionary
computation, and the PushGP genetic programming system, implemented in
Clojure. More information about Push and PushGP can be found at
http://hampshire.edu/lspector/push.html.

Clojush derives mainly from Push3 (for more information see
http://hampshire.edu/lspector/push3-description.html,
http://hampshire.edu/lspector/pubs/push3-gecco2005.pdf) but it is not
intended to be fully compliant with the Push3 standard and there are a
few intentional differences. It was derived most directly from the Scheme
implementation of Push/PushGP (called Schush). There are several differences
between Clojush and other versions of Push3 -- for example, almost all of the
instruction names are different because the `.` character has special
significance in Clojure -- and these are listed below.

If you want to understand the motivations for the development of Push, and
the variety of things that it can be used for, you should read a selection of
the documents listed at http://hampshire.edu/lspector/push.html, probably
starting with the 2002 "Genetic Programming and Evolvable Machines" article
that can be found at http://hampshire.edu/lspector/pubs/push-gpem-final.pdf.
Bear in mind that Push has changed over the years, and that Clojush is
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

Starting with version 2.0.0, the genomes of evolving individuals in Clojush are based on Plush (linear Push) genomes, which are translated into normal Push programs before execution. Plush genomes are composed of instruction maps, each of which contains an instruction and potentially other metadata describing whether that instruction should be silenced, whether closing parentheses should follow it, etc.

Usage
-----

Example calls to PushGP are provided in other accompanying files.

Push programs are run calling `run-push`, which takes as arguments a Push
program and a Push interpreter state that can be made with `make-push-state`.
If you are planning to use PushGP then you will want to use this in the error
function (a.k.a. fitness function) that you pass to the `pushgp` function.
Here is a simple example of a call to `run-push`, adding 1 and 2 and returning
the top of the integer stack in the resulting interpreter state:

    (top-item :integer (run-push '(1 2 integer_add) (make-push-state)))

If you want to see every step of execution you can pass an optional third
argument of `true` to `run-push`. This will cause a representation of the
interpreter state to be printed at the start of execution and after
each step. Here is the same example as above but with each step printed:

    (top-item :integer (run-push '(1 2 integer_add) (make-push-state) true))

See the "parameters" section of the code for some parameters that will affect
execution, e.g. whether code is pushed onto and/or popped off of the code
stack prior to/after execution, along with the evaluation limits (which can be
necessary for halting otherwise-infinite loops, etc.).

`Run-push` returns the Push state that results from the program execution; this
is a Clojure map mapping type names to data stacks. In addition, the map
returned from `run-push` will map `:termination` to `:normal` if termination was
normal, or `:abnormal` otherwise (which generally means that execution was
aborted because the evaluation limit was reached.

Random code can be generated with `random-code`, which takes a size limit and a
list of "atom generators." Size is simply the length of the linear Plush genome.
Each `atom-generator` should
be a constant or the name of a Push instruction (in which case it will be
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

```clojure
(let [s (make-push-state)
      c (random-push-code
          100                                  ;; size limit of 100 points
          (concat @registered-instructions     ;; all registered instrs
                  (list (fn [] (rand-int 100)) ;; random integers from 0-99
                        (fn [] (rand)))))]     ;; random floats from 0.0-1.0
  (printf "\n\nCode: %s\n\n" (apply list c))
  (run-push c s))
```

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

The `pushgp` function is used to run PushGP. It takes all of its parameters
as keyword arguments, and provides default values for any parameters that are
not provided. See the `pushgp` definition in `pushgp/pushgp.clj` for details. The single
argument that must be provided is `:error-function`, which should be a function that
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
play with. The `registered-for-type` function can make it simpler to include
or exclude groups of instructions. This is demonstrated in some of the
examples.

As of Clojush 2.0.0, genetic operator arguments are provided as a map to the `:genetic-operator-probabilities` argument. Here, each key may be a single operator or an "operator pipeline" vector, which allows the application of multiple operators sequentially, using one operators output as the input to the next operator. An example argument could be:

```clojure
{:reproduction 0.1
 :alternation 0.2
 :uniform-mutation 0.2
 [:alternation :uniform-mutation] 0.2
 :uniform-close-mutation 0.1
 :uniform-silence-mutation 0.1
 [:make-next-operator-revertable :uniform-silence-mutation] 0.1}
```

Here, two different pipelines would be used. In the second pipeline, the meta-operator `:make-next-operator-revertable` makes the `:uniform-silence-mutation` operator revertable, which means that the child will be compared to the parent, and the parent kept if it is better than the child.

The use of simplification is also novel here. Push programs can be automatically
simplified -- to some extent -- in a very straightforward way: because
there are almost no syntax constraints you can remove anything (one or more
atoms or sub-lists, or a pair of parentheses) and still have a valid
program. So the automatic simplification procedure just iteratively removes
something, checks to see what that does to the error, and keeps the simpler
program if the error is the same (or lower!).

Automatic simplification is used in this implementation of PushGP in two places:

1. A specified number of simplification iterations is performed on the best
program in each generation. This is produced only for the sake of the report,
and the result is not added to the population. It is possible that the
simplified program that is displayed will actually be better than the best
program in the population. Note also that the other data in the report
concerning the "best" program refers to the unsimplified program.
2. Simplification is also performed on solutions at the ends of runs.

Note that the automatic simplification procedure will not always find all
possible simplifications even if you run it for a large number of iterations,
but in practice it does often seem to eliminate a lot of useless code (and to
make it easier to perform further simplification by hand).

If you've read this far then the best way to go further is probably to read
and run the example problem files in `src/clojush/problems/demos`.

Implementation Notes
--------------------

A Push interpreter state is represented here as a Clojure map that maps type
names (keywords) to stacks (lists, with the top items listed first).

Push instructions are names of Clojure functions that take a Push
interpreter state as an argument and return it modified appropriately. The
`define-registered` macro is used to establish the definitions and
also to record the instructions in the global list registered-instructions.
Most instructions that work the same way for more than one type are
implemented using a higher-order function that takes a type and returns a
function that takes an interpreter state and modifies it appropriately.
For example: there's a function called `popper` that takes a type and returns
a function -- that function takes a state and pops the right stack in the
state. This allows us to define `integer_pop` with a simple form:

    (define-registered integer_pop (popper :integer))

In many versions of Push `RUNPUSH` takes initialization code or initial stack
contents, along with a variety of other parameters. The implementation of
`run-push` here takes only the code to be run and the state to modify. Other
parameters are set globally in the parameters section below. At some point
some of these may be turned into arguments to `run-push` so that they aren't
global.

Miscellaneous differences between clojush and Push3 as described in the Push3
specification:

- Clojush instruction names use `_` instead of `.` since the latter has
  special meaning when used in Clojure symbols.
- Equality instructions use `eq` rather than `=` since the latter in not
  explicitly allowed in Clojure symbols.
- for similar reasons `+, -, *, /, %, <`, and `>` are replaced with `add, sub,
  mult, div, mod, lt`, and `gt`.
- Boolean literals are `true` and `false` (instead of `TRUE` and `FALSE` in
  the Push3 spec). The original design decision was based on the fact that
  Common Lisp's native Boolean literals couldn't used without conflating
  false and the empty list (both `NIL` in Common Lisp).
- Clojush adds `exec_noop` (same as `code_noop`).
- Clojush includes an execution time limit (via the parameter
  `evalpush-time-limit`) that may save you from exponential code growth or other
  hazards. But don't forget to increase it if you expect legitimate programs
  to take a long time.

Push3 stuff not (yet) implemented:
- `NAME` type/stack/instructions
- Other missing instructions: `*.DEFINE`, `CODE.DEFINITION`, `CODE.INSTRUCTIONS`
- The configuration code and configuration files described in the Push3
  spec have not been implemented here. The approach here is quite different,
  so this may never be implemented

[How to Contribute](./CONTRIBUTING.md)
--------------------------------------

To Do (sometime, maybe)
-----------------------

- Implement remaining instructions in the Push3 specification.
- Add support for seeding the random number generator.
- Improve the automatic simplification algorithm and make it work on Plush genomes.
- Possibly rename the auxiliary stack the "input" stack if no other
  uses are developed for it.
- Write a `sufficient-args` fn/macro to clean up Push instruction definitions.

Acknowledgements
----------------

This material is based upon work supported by the National Science Foundation
under Grant No. 1017817. Any opinions, findings, and conclusions or
recommendations expressed in this publication are those of the authors and
do not necessarily reflect the views of the National Science Foundation.
