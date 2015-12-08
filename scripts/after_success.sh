#!/usr/bin/env bash
set -o errexit
set -o nounset
set -o xtrace
set -o pipefail

LAST_COMMIT_AUTHOR_EMAIL=$(git --no-pager show -s --format='%ae' HEAD)
echo "last commit ID: $LAST_COMMIT_AUTHOR_EMAIL"
AUTOMATED_AUTHOR_EMAIL=_@_._
AUTOMATED_AUTHOR_NAME=_

# if this commit was a created automatically (by lein release) then we dont
# wanna re release it. If it is, exit.
test $TRAVIS_PULL_REQUEST == "false" && \
  test $TRAVIS_BRANCH == "master" &&
  test $LAST_COMMIT_AUTHOR_EMAIL != $AUTOMATED_AUTHOR_EMAIL

git remote set-url origin https://$GITHUB_TOKEN@github.com/$TRAVIS_REPO_SLUG.git
git branch --set-upstream-to origin/master master
git config user.name "$AUTOMATED_AUTHOR_NAME"
git config user.email "$AUTOMATED_AUTHOR_EMAIL"
git config push.default simple
# dont output all of lein doc, because its overly long because it tries
# to run experiments
lein doc 2>&1 | head -n 100
./scripts/deploy-docs.sh --verbose
git checkout master
git status
git branch
lein release
git tag
