#!/usr/bin/env bash
set -e

if [[ "$CI" != "" ]]; then
  tox -e devruntime
fi
tox --skip-missing-interpreters
