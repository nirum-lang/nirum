Nirum
=====

[![Build Status (Travis CI)][ci-svg]][ci]
[![Build Status (AppVeyor)][ciw-svg]][ciw]
[![Docker Automated Build][docker-svg]][docker]
[![Gitter][chat-svg]][chat]

[ci-svg]: https://travis-ci.org/spoqa/nirum.svg
[ci]: https://travis-ci.org/spoqa/nirum
[ciw-svg]: https://ci.appveyor.com/api/projects/status/jf9bsrnalcb1xrp0?svg=true
[ciw]: https://ci.appveyor.com/project/dahlia/nirum-k5n5y
[docker]: https://hub.docker.com/r/spoqa/nirum/
[docker-svg]: https://img.shields.io/docker/automated/spoqa/nirum.svg
[chat-svg]: https://badges.gitter.im/spoqa/nirum.svg
[chat]: https://gitter.im/spoqa/nirum?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge

Nirum is an [IDL][1] compiler and [RPC][2]/[distributed object][3] framework
for [microservices][4], built on top of the modern Web server technologies
such as RESTful HTTP and JSON.

You can find how its IDL looks like from source codes in the `examples/`
directory.

**Note that its design is highly unstable and could be changed.**
Also the feature set is incomplete yet.

[1]: https://en.wikipedia.org/wiki/Interface_description_language
[2]: https://en.wikipedia.org/wiki/Remote_procedure_call
[3]: https://en.wikipedia.org/wiki/Distributed_object
[4]: https://en.wikipedia.org/wiki/Microservices


Nightly builds
--------------

The easiest way to give a try to use Nirum is downloading a nightly build.
We currently provides the prebuilt binaries of the following platforms:

- [Linux (x86_64)](https://nightly-builds.nirum.org/travis-builds/nirum-linux-x86_64)
- [Mac (x86_64)](https://nightly-builds.nirum.org/travis-builds/nirum-darwin-x86_64)
- [Windows (x64)](https://ci.appveyor.com/api/projects/dahlia/nirum-k5n5y/artifacts/nirum-win-x64.exe?job=Platform%3A%20x64&branch=master)
- [Windows (x86)](https://ci.appveyor.com/api/projects/dahlia/nirum-k5n5y/artifacts/nirum-win-x86.exe?job=Platform%3A%20x86&branch=master)
- [Docker (`spoqa/nirum:latest`)][docker]


Getting started
---------------

In order to compile a Nirum package (`examples/`) to a Python package:

    $ mkdir out/  # directory to place generated Python files
    $ nirum -o out/ examples/

For more infomration, use `--help` option:

    $ nirum --help
    Nirum Compiler 0.3.0

    nirum [OPTIONS] DIR

    Common flags:
      -o --output-dir=DIR   The directory to place object files
      -t --target=TARGET    The target language.  Available targets: python
      -? --help             Display help message
      -V --version          Print version information
         --numeric-version  Print just the version number


Building
--------

If you already installed [Haskell Platform][5] or [Haskell Stack][6],
you can build the project in the same way to build other Haskell projects.

Using Haskell Stack:

    $ stack build

Using vanilla Cabal:

    $ cabal sandbox init
    $ cabal install --only-dependencies
    $ cabal configure
    $ cabal build

You can run the test suite of Nirum:

    $ stack test  # using Hasekll Stack
    $ cabal test  # using Haskell Platform
    $ ./lint.sh   # run style lint as well

[5]: https://www.haskell.org/platform/
[6]: https://www.haskellstack.org/


Etymology
---------

**니름** (IPA: /niɾɯm/; *nireum*) is a sort of telepathy in the fictional world
of [The Bird That Drinks Tears][7] (눈물을 마시는 새 *Nunmureul masineun sae*)
by [Lee Yeongdo][8] (이영도).

[7]: https://en.wikipedia.org/wiki/The_Bird_That_Drinks_Tears
[8]: https://en.wikipedia.org/wiki/Lee_Yeongdo
