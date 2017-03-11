#!/usr/bin/env bash
set -e

stack build
stack exec -- nirum -o nirum_fixture -t python test/nirum_fixture

tox --skip-missing-interpreters
