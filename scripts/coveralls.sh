#!/usr/bin/env bash
# based on http://bfontaine.net/blog/2014/02/15/using-coveralls-with-clojure/

set -o errexit
set -o nounset
set -o xtrace
set -o pipefail

# don't run problem tests with coverage, because they break it with
# duplicate instruction problems
PROBLEM_TESTS=test/clojush/midje/problems/
TMP_DIR=tmp_ignore/
mv $PROBLEM_TESTS $TMP_DIR

COVERALLS_URL='https://coveralls.io/api/v1/jobs'
lein cloverage -o cov --coveralls --ns-exclude-regex '^clojush[.]problems.*'
curl -F 'json_file=@cov/coveralls.json' "$COVERALLS_URL"

mv $TMP_DIR $PROBLEM_TESTS
