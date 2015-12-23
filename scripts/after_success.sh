#!/usr/bin/env bash
set -o errexit
set -o nounset
set -o xtrace
set -o pipefail

AUTOMATED_AUTHOR_EMAIL=_@_._
AUTOMATED_AUTHOR_NAME=_

git remote set-url origin https://$GITHUB_TOKEN@github.com/$TRAVIS_REPO_SLUG.git
git branch --set-upstream-to origin/master master
git config user.name "$AUTOMATED_AUTHOR_NAME"
git config user.email "$AUTOMATED_AUTHOR_EMAIL"
git config push.default simple
# dont output all of lein doc, because its overly long because it tries
# to run experiments
lein codox 2>&1 | head -n 100
./scripts/deploy-docs.sh --verbose
git checkout master
lein release
