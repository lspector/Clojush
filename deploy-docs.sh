#!/usr/bin/env bash

set -o errexit
set -o pipefail
set -o nounset

# print all lines of script as they are run
set -o xtrace

lein doc

env GIT_DEPLOY_REPO=https://$GITHUB_TOKEN@github.com/$TRAVIS_REPO_SLUG.git GIT_DEPLOY_DIR=doc GIT_DEPLOY_EMAIL=_@_ ./deploy.sh --verbose
