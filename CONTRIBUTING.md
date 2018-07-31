Contirubtion guide
==================

Build
-----

In the same way to other Haskell softwares, Nirum compiler can be built using
[Haskell Stack].  If you didn't install it yet see also its
[installation guide][1].

If it's your first build of Nirum compiler you need to setup a proper version
of GHC:

~~~~~~~~ bash
stack setup
~~~~~~~~

The following command builds an executable binary of Nirum compiler:

~~~~~~~~ bash
stack build
~~~~~~~~

A built executable binary aren't installed to `PATH` until `stack install`
command is run.  Without installation a built executable binary can be invoked
using `stack exec` command:

~~~~~~~~ bash
stack exec -- nirum -t python -o out/ examples/
~~~~~~~~

Note that `--` indicates options after it belong to the `nirum` executable,
not `stack exec` command.

[Haskell Stack]: https://haskellstack.org/
[1]: https://docs.haskellstack.org/en/stable/install_and_upgrade/


Testing scopes
--------------

Since it is a compiler which generate source codes, there are two scopes
we should test:

 1. the compiler itself, and
 2. codes that the compiler generates.

The former is unit testing and the latter one is integration testing.  We have
two commands/scripts to run corresponding testing:

 1. `stack test :spec`
 2. `stack test :targets`

If you've changed things related to a target backend you should test everything:

~~~~~~~~ bash
stack test
~~~~~~~~

If you've changed pure internals of the compiler probably it's okay
to run only the former command.  Anyway both testings are run by CI so that
changes breaking either testing cannot be merged.

[![Build Status (Travis CI)][ci-svg]][ci]
[![Build Status (AppVeyor)][ciw-svg]][ciw]

[ci-svg]: https://travis-ci.org/spoqa/nirum.svg?branch=master
[ci]: https://travis-ci.org/spoqa/nirum
[ciw-svg]: https://ci.appveyor.com/api/projects/status/jf9bsrnalcb1xrp0?svg=true
[ciw]: https://ci.appveyor.com/project/dahlia/nirum-k5n5y


External dependencies
---------------------

Each target test suite has its external dependencies (i.e., non-library program
dependencies):

 -  Python
     -  All Python versions that Nirum should support: Python 2.7, and
        3.4 and above all
     -  [`tox`][tox] 3.0.0 or higher

[tox]: https://tox.readthedocs.io/


Lint
----

Since we want to keep our coding style consistently, we run [lint] to check
it.  You can get more information about [hlint] on its homepage.

We recommend you to register lint to Git hooks.

~~~~~~~~ bash
ln -s "$PWD/lint.sh" "$PWD/.git/hooks/pre-commit"
~~~~~~~~

If you have registered a hook once, it will be automatically executed
when you make a commit.


Changelog
---------

We believe logging changes is a part of making software.
So we have the policy that enforcing every pull request has a log to changelog
(i.e., a diff on *CHANGES.md* file) on the [CI][changelog-test-ci].

Please put `[chagelog skip]` on commit message if your change is not related to
the Nirum compiler (e.g., fixing a typo).


[lint]: https://en.wikipedia.org/wiki/Lint_(software)
[hlint]: https://github.com/ndmitchell/hlint
[changelog-test-ci]: https://github.com/spoqa/nirum/blob/454e4b6f9f934c8cae515b0e4c7a2acf6ba32891/.travis.yml#L45-L55
