#!/usr/bin/env bash

stack build && stack exec -- nirum -o nirum_fixture test/nirum_fixture
pushd "nirum_fixture";
  pip install -e .
popd;

pytest test/python
