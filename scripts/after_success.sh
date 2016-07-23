#!/usr/bin/env bash
set -o errexit
set -o nounset
set -o xtrace
set -o pipefail

# exit if not on master branch
[ $TRAVIS_BRANCH = master ]

AUTOMATED_AUTHOR_EMAIL=_@_._
AUTOMATED_AUTHOR_NAME=_
LEIN_RELEASE_COMMAND=$(git log --format=%B -n 1 | grep -o 'lein release :[a-z]*')

# exit if no lein release
[ -n "$LEIN_RELEASE_COMMAND" ]

git remote set-url origin https://$GITHUB_TOKEN@github.com/$TRAVIS_REPO_SLUG.git
git branch --set-upstream-to origin/master master
git config user.name "$AUTOMATED_AUTHOR_NAME"
git config user.email "$AUTOMATED_AUTHOR_EMAIL"
git config push.default simple
# dont output all of lein doc, because its overly long because it tries
# to run experiments
lein codox 2>&1 | head -n 100
git pull origin gh-pages
./scripts/deploy-docs.sh --verbose
git checkout master
git pull origin master
eval $LEIN_RELEASE_COMMAND
