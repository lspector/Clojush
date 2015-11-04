#!/usr/bin/env bash

set -o errexit
set -o pipefail
set -o nounset

# dont print all lines because we dont wanna print the github tokem
# set -o xtrace

git remote add origin https://$GITHUB_TOKEN@github.com/$TRAVIS_REPO_SLUG.git
git branch --set-upstream-to origin/master

lein release
