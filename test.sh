#!/usr/bin/env bash
set -e

stack build
python_outdir="nirum_fixture"
stack exec -- nirum -o "$python_outdir" -t python test/nirum_fixture
echo "*" > "$python_outdir/.gitignore"

tox --skip-missing-interpreters
