# Contributing

[Here](https://gist.github.com/thelmuth/1361411) is a document describrining how
to contribute to this project.

## Travic CI
Recently we have begun using [Travis CI](travis-ci.org) to automate multiple
parts of development.

### Testing

Primarily it serves as a way to test every branch and pull request, using commands
like `lein check` and `lein test`. Currently, the test cases are very limited
and do not cover much of the codebase.

### Docs

Docs are auto generated from function metadata using
[`codox`](https://github.com/weavejester/codox).

On every commit to master, the docs are automatically regenerated and pushed
to the [`gh-pages` branch](http://lspector.github.io/Clojush/).

To generate them locally run `lein doc` and then open `doc/index.html`.

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

Currently it will always bump a minor version. It
