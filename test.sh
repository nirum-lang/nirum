#!/usr/bin/env bash
set -e

stack build
stack exec -- nirum -o nirum_fixture test/nirum_fixture

tox
