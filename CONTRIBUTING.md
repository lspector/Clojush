# Contributing

[Here](https://gist.github.com/thelmuth/1361411) is a document describrining how
to contribute to this project.

## Testing


### Integration tests

The main tests for the codebase are integration tests. They run Clojush
with a couple of different configurations, and verify that the output (CSV, JSON,
EDN, as well as text) is the same as it is supposed to be. You can run them
with:

```bash
# all integeration tests
lein test clojush.test.core-test

# just one configuration
lein test :only clojush.test.core-test/<label>
```

If you change
anything about how Clojush outputs data or computes things, they are likely to
fail. You will need to regenerate the saved output with:

```bash
lein run -m clojush.test.core-test/regenerate [<label> ...]
```

Since there are some things that will always change (like the time and git hash)
there is some manual find and replace logic in `clojush.test.integration-test`
that tries to replace things (like the git hash) will change with `xxx` in the test output.

### Benchmarks

If you are trying to improve the performance of the codebase, or want to check if a change adversely affects performance,
you should run the benchmarks!

#### Design

The goal of the benchmarks in this codebase is to help us assess how changes to the codebase will effect the time it
takes to execute a run. Right now, we have decided to use the RSWN problem with autoconstruction as the representative
problem (see `clojush.bench.helpers/call-main` for the full args used).

To make this more concrete, we want to know the mean time it takes to execute a run before and after some code change.
The ["Rigorous Benchmarking in Reasonable Time" (Klibera, Jones)](https://kar.kent.ac.uk/33611/7/paper.pdf) gives us
a statistical framework for assessing the certainty of a certain performance change. It gives an estimator for the
ratio of the new mean time to the old mean time, with a confidence interval, without any assumptions about the underlying
distributions of the times. They give a formula that generalizes to multiple sampling levels, but we only implement
the limit case of just one sampling level in `clojush.bench.compare/ratio`. This is sufficient to be a useful starting
place for us.

So what should we actually be benchmarking? The most accurate and easiest choice would be to execute a full run.
However, this is just too time intensive, as one run easily takes hours and we would need likely at least 10 to 
get any sense of a change. We would likely need more, to get a reliable measure of the performance difference, because
each run is stochastic and the running time varies greatly. Not only because some runs finish earlier than others,
but also because each generation itself takes a varying amount of time, depending on the characteristics of the population.

So, instead of comparing the performance of a whole run between code changes, we instead choose a smaller part of the
run, and compare the performance of this part, given the same initial data. The largest chunk we measure is the 
`clojush.pushgp.pushgp/process-generation` function, which processes a whole generation. By saving the input
to this function and executing it on both code bases, we can see how they would perform on that certain generation.

We can also profile other functions, like `clojush.interpreter/eval-push`, to see how their performance changes
in isolation. This is helpful if we are working on just optimizing one functions. We can iterate on the code
and just run the benchmarks against that function, before running them on the total generations, to see the overall change.

This technique has been successfull so far, because it allows us to execute our benchmarks in a reasonable amnount of
time (<30 mins) and the times have a small enough varience that we can get a very precise estimate of the speedup.


#### Implementation

How do we actually execute the benchmarks? We use the [lein-jmh](https://github.com/jgpc42/lein-jmh) library
that wraps the Java Microbenchmark Harness (JMH). This allows us to easily run each benchmarked function in it's own
process and do some number of warmup runs.

The use of mutable state and globals adds a few pain points, but we are able to work around them. In order to make
sure the globals are setup properly (like with the right push functions and such), we start
a run in each provess before executing any benchmarks and stop it right after it sets the globals. Also, some functions
(like `clojush.pushgp.pushgp/process-generation`) take mutable inputs (agents, RNG). Since we execute this function
multiple times on the same inputs, we actually serialize them with just their immutable parts extracted, and then
before each execution we re-wrap them in mutable containers.

When we sample from the inputs, we serialize those using Java's built in serializer. We sample some (configurable)
percentage of all calls.

The benchmarks should be run on a machine that doesn't have other work going on, ideally not a laptop.
If you need a machine, you should talk to Saul about running them on `deucalon` (a machine at Hampshire).


We could improve the process in a number of ways:

* Implement the multi level formula for the ratios and execute each benchmark in multiple forks, and then in each
  fork multiple times, as the different levels.
* Take samples from more than one configuration, to understand how changes affect different types of runs.
* Record generation number for each sample, so we can see how things improve at different points in the run.
* Stop using `lein-jmh` and use `clojure-jmh` library directly, so we can remove some of the code duplication
  in `jmh.edn`. 

#### Running existing benchmarks

First, generate the sample inputs:

```bash
lein run -m clojush.bench.helpers/sample
```

This saves the inputs for each benchmarked function in `./bench-inputs/<function name>/<random id>`.

Then, you should edit the `params` in `jmh.edn` to match the id generated for each sampled functions.

Finally, you can run the benchmarks:

```bash
lein jmh '{:status true}'
```

When they finish, it should output some EDN that has the results. Save that to a file.

Then, run the benchmarks again with your code changes and save that to another file. Then you can 
compare the performance between the two with:

```bash
lein run -m clojush.bench.compare old.edn new.edn
```

This shows you, for each function and sample input, the ratio of the means of the execution time
of the new to the old. It also has a 95% CI. If this is below 1.0, then this means the new
execution is faster.

#### Benchmarking new functions

If you are trying to improve the performance of a certain function, you should create a benchmark for it.

First, add a map to the `clojush.bench.helpers/sampled-functions` at least the keys `:fn-var` and `:save-prob`, 
which correspond to the var of the function and the probability you want to save a sample of the inputs when you are
executing it.

Then run `lein run -m clojush.bench.helpers/sample` to gather samples for the file.

Then add it to the `jmh.edn` file. Create a new item in `:benchmarks`, mirroring the existing ones in there.
If the function is normally executed concurrently during a run (like `eval-push`), set the `:threads` to be close
to the number of CPUs on your benchmarking machine.

You have to decide how many times the function should be run together and times. This is the `:count` key in `:measurement`
and `:warmup`. If it takes < 1ms to execute the function, you should make this greater than 1, so that the overhead from
the timing code doesn't overwhelm the function time. How do you decide how much warmup the function needs?
Run it without any warmup and with some large number of iterations and then see when the time per iteration changes. The
easiest way to do this is to extract out the time per iteration with something like this and plot the values:

```bash
lein trampoline jmh "{:status true :select [:bechmark-name]}"  2> /dev/null | sed -ln 's/Iteration.*: \(.*\) ms\/op/\1/p'
```


## Travis CI

Recently we have begun using [Travis CI](travis-ci.org) to automate multiple
parts of development.

### Docs

Docs are auto generated from function metadata using
[`codox`](https://github.com/weavejester/codox).

On every commit to master, the docs are automatically regenerated and pushed
to the [`gh-pages` branch](http://lspector.github.io/Clojush/).

To generate them locally run `lein codox` and then open `doc/index.html`.

Currently, generating the docs have the side effect of running some examples,
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
