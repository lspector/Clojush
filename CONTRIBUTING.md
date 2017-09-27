# Contributing

[Here](https://gist.github.com/thelmuth/1361411) is a document describrining how
to contribute to this project.

## Testing

Primarily it serves as a way to test every branch and pull request, using commands
like `lein check` and `lein test`.

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
that tries to replace things will change with `xxx` in the test output.

### Benchmarks

We use the [libra](https://github.com/totakke/libra) library to define tests
(some using [Criterium](https://github.com/hugoduncan/criterium)):

```bash
$ lein libra

Measuring clojush.interpreter-bench

eval-push-on-1000-from-clojush.problems.software.replace-space-with-newline (interpreter_bench.clj:46)
Grabbing executions...
Running benchmark...

time: 805.688731 ms, sd: 202.088504 µs

...
```

## Travic CI

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
