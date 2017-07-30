# Contributing

[Here](https://gist.github.com/thelmuth/1361411) is a document describrining how
to contribute to this project.

## Logging

We break up the logging of the run into a series of **events**, each
identified by a label.
We call `clojush.graphs/log!` with both the label of the event and the input
data.

We use
[Plumatic's Graph framework](https://github.com/plumatic/plumbing#graph-the-functional-swiss-army-knife)
to compute data for that event. We define each event's graph
in `clojush.graphs.events.<label>/compute-graph`, They are compiled with `lazy-compile`.
so that their outputs are lazy maps. This means that the values are only calculated
when we ask for them.
We execute the compiled graph for that event with the input
passed into `log!` merged with a mapping of each previouslly logged event label to it's computed
data.

Then, we calls all the **handlers** that have handle functions defined
for this event. We define the mapping of event label to handle functions
in `clojush.graphs.handlers.<handler name>/handler`. Each handle function
is called with a mapping of each event label to it's last computed result.

You can see what each event graph takes in as inputs and what it produces,
by running (respectively):

```bash
lein run -m clojush.cli.log/event-inputs
lein run -m clojush.cli.log/event-outputs
```

### Population
There are a bunch of bits things we might want to know about each
individual in the population for logging purposes. These might
be as simple as the mean of it's error vector and as complicated
as program string of the partially simplified version of itself. Some
of these things we just want to know about the best individual and others
about all individuals, and we never want to compute them unless we need to.

So the `generation` event computes an augmented `population`, where
each item has all the original keys of the individual plus some extra
lazy dynamic ones. It does this by, again, computing a lazy map based on
a graph. It is defined at `clojush.graphs.events.generation.individual/compute-graph`
and takes as input the original individual, under the key `:individual` as
well as the argmap and some computed stats on the population. The lazy map output
is merged with the original individual record. You can see the input and
output of this graph with:

```bash
lein run -m clojush.cli.log/individual-input
lein run -m clojush.cli.log/individual-output
```

Since we need these extra values on the "best" individual, for logging,
we compute the best from this augmented population.

After the generation finished, the pushgp function needs to know if
we have succeeded and get the "best" individual, so that it can return it.
So it get's the computed data from the generation and accesses
the `outcome` and the `best`.

### Modifying Logging

#### Adding computed data
OK let's say you want to log some more data during the run. First, decide
which graph it should be in:

* Depends on the command line arguments? Then it belongs in `init`
* Computes something about the machine environment (like git hash) or depends
  on the push argmap? Put that in `config`.
* Computed ever generation and is population wide? -> `generation`
* Computed for each individual in every generation? -> `individual`

Then, add it to that graph. The most straightforward way to do that
is to define a keyword function (`defnk`) in that file and put
that keyword function in the `compute-graph` in that same file. Use the above
logic and CLI commands to understand what you can ask for as input in the `defnk`.


#### Adding a handler
If you want to create a new handler to support logging in some new format
or new source, you should:

1. Add a toggle for the handler in `clojush.args/argmap`.
2. Create a `clojush.graphs.handlers.<label>` file. In it, define a `handler`
   var that maps from event labels to handle functions. Make those handle
   funtions execute based on the toggle you defined in the argmap.
3. Add that handler to `clojush.graphs.handlers/handlers`.


#### Example
For exmaple, let's create a handler that logs, to a file, the number of empty
genomes in each population every 20 generations.

First we add a couple of options to `clojush.args/argmap`:

```clojure
:print-empty-genome-logs false
:empty-genome-logs-every-n-generations 20
:emtpy-genome-logs-filename "empty-genomy-logs.txt"
```

Then, let's make a file for this handler at `clojush.graphs.handlers.empty-genome`:

```clojure
(ns clojush.graphs.handlers.empty-genome
  (:require [plumbing.core :refer [defnk]]
            [clojure.java.io :as io]))

(defnk handle-config
  "Save the header of the file before the run starts"
  [[:config [:argmap print-empty-genome-logs emtpy-genome-logs-filename]]]
  (when print-empty-genome-logs
    (spit emtpy-genome-logs-filename "Generation NumEmptyGenomes\n")))

(defnk handle-generation
  "At every generation, if it's the nth generation, save the # of emtpy genomes"
  [[:config [:argmap print-empty-genome-logs
                     emtpy-genome-logs-filename
                     empty-genome-logs-every-n-generations]]
   [:generation index :as generation]]
  (when (and print-empty-genome-logs
             (= 0 (mod index empty-genome-logs-every-n-generations)))
    (spit
      emtpy-genome-logs-filename
      (str index " " (:empty-genomes-n generation))
      :append true)))

(def handler
  {:config handle-config
   :generation handle-generation})
```

Then add that `handler` to `clojush.graphs.handlers/handlers`. As you can
see, I didn't actually do any computation in the handler to figure out
the `empty-genomes-n`. Instead, I just asked for that value from the
`generation`. So let's define this key on this generation. First
we add a new keyword funciton in `clojush.graphs.events.generation`:

```clojure
(defnk empty-genomes-n [population]
  (count (filter #(empty? (:genome %)) population)))


(def compute-graph
  (graph/graph
    ...

    empty-genomes-n))
```

We are getting the `population` that is also computed by a different keyword
function.

Then we add that keyword function to `clojush.graphs.events.generation/compute-graph`.
It infers the name by looking at the name of the keyword function.

By moving the computation out of the handler, any other handler can also access
this attribute now of the generation.

One other thing we could do to clean this up is to add a `genome-empty?` value on
each individual. To do this, add a keyword function in `clojush.graphs.events.generation.individual`:

```clojure
(defnk empty-genome? [[:individual genome]]
  (empty? genome))

(def compute-graph
  (graph/graph
    ...
    empty-genome?))
```

Then we can clean up the generation level attribute:

```clojure
(defnk empty-genomes-n [population]
  (count (filter :empty-genome? population)))
```

### Debugging the graph

If you set the `CLOJUSH_DEBUG_GRAPH` environmental variables, then it will
print to stderr when all values in the graph are being calculated.


### Profiling the graph

We can also produce a flame graph for the run, to understand where time is being
spent. First, download the [FlameGraph](https://github.com/brendangregg/FlameGraph)
library. Then, do a run with the `CLOJUSH_FLAME_GRAPH_FILE` set to whatever
file you want the profiling output to be. Then you can `flamegraph.pl` on that
file to get an SVG output.

```bash
git clone git@github.com:brendangregg/FlameGraph.git ~/FlameGraph
env CLOJUSH_FLAME_GRAPH_FILE=profile.kern_folded lein run clojush.problems.integer-regression.nth-prime
~/FlameGraph/flamegraph.pl profile.kern_folded > profile.svg
open profile.svg
```

## Travic CI

We use [Travis CI](travis-ci.org) for...

### Testing

It tests every branch and pull request, using commands
like `lein check` and `lein test`.


#### Integration tests
The main tests for the codebase are integration tests. They run Clojush
with a couple of different configurations, and verify that the output (CSV, JSON,
EDN, as well as text) is the same as it is supposed to be. You can run them
with:

```bash
# all integeration tests
lein test clojush.test.integration-test

# just one configuration
lein test :only clojush.test.integration-test/<label>
```
If you change
anything about how Clojush outputs data or computes things, they are likely to
fail. You will need to regenerate the saved output with:

```bash
lein run -m clojush.test.integration-test/regenerate [<label> ...]
```

If the tests are not passing, because something has changed, I often find it easier to regenerate
the test ouputs then use `git diff` to see what has changed, instead of having the Clojure
test checker do the diff.

Since there are some things that will always change (like the time and git hash)
there is some manual find and replace logic in `clojush.test.integration-test`
that tries to replace things will change with `xxx` in the test output.


### Docs

The docs are auto generated from function metadata using
[`codox`](https://github.com/weavejester/codox).

On every commit to master, the docs are automatically regenerated and pushed
to the [`gh-pages` branch](http://lspector.github.io/Clojush/).

To generate them locally run `lein codox` and then open `doc/index.html`.

Generating the docs have the side effect of running some examples,
[because I couldn't figure out how stop codox from loading all example files](https://github.com/weavejester/codox/issues/100).

In the metadata, you can [skip functions](https://github.com/weavejester/codox#metadata-options)
and also [link to other functions](https://github.com/weavejester/codox#docstring-formats).

In order for this work, you must set the `GITHUB_TOKEN` environmental variable
in the [repository settings in Travis](http://docs.travis-ci.com/user/environment-variables/#Defining-Variables-in-Repository-Settings).
It needs this so it can push the updated docs back to Github.

### Releases

We use [the `lein release` command](https://github.com/technomancy/leiningen/blob/master/doc/DEPLOY.md#releasing-simplified)
to add a new release on every build on the `master` branch. Check the
`:release-tasks` key in the [`project.clj`](./project.clj) for a list of
all steps it takes.

This requires setting the `LEIN_USERNAME` and `LEIN_PASSWORD` in
the [repository settings in Travis](http://docs.travis-ci.com/user/environment-variables/#Defining-Variables-in-Repository-Settings),
so that it can the release to Clojars. It also needs the `GITHUB_TOKEN`
in order to push the added tag and commit back to Github.

Travis will:

1. Build docs + push those to `gh-pages` branch after every master branch build with the keyword `lein release $LEVEL`
   For example, you could have a commit message like `add some stuff lein release :patch` which would cause it to do a patch release.
   The available levels are listed in [the lein docs](https://github.com/technomancy/leiningen/blob/stable/doc/DEPLOY.md#releasing-simplified)
2. Run `lein release $LEVEL` as well on those builds. This will:
    1. create new git commit and tag for new commit w/ out `-SNAPSHOT` in it
    2. Create jar and push that to clojars
    3. bump release number to next minor version
    4. Push new commits + tag back to github
