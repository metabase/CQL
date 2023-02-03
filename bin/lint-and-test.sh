#! /usr/bin/env bash

# This script is a convenience for running linters and tests without having to type in a bunch of nonsense.

set -euxo pipefail

clj-kondo --parallel --lint src test

clojure -X:dev:test $@
