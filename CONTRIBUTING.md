Contirubtion guide
==================

Build
-----

In the same way other Haskell softwares, Nirum compiler can be built using
[Haskell Stack][].  If you didn't install it yet see also its
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
