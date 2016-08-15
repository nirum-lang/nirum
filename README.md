Nirum
=====

[![Build Status][ci-svg]][ci]

[ci-svg]: https://travis-ci.org/spoqa/nirum.svg
[ci]: https://travis-ci.org/spoqa/nirum

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

- [Linux (x86_64)](http://nightly-builds.nirum.org/travis-builds/nirum-linux-x86_64)


Getting Started
---------------

In order to compile a Nirum package (`examples/`) to a Python package:

    $ mkdir out/  # directory to place generated Python files
    $ nirum -o out/ examples/

For more infomration, use `--help` option:

    $ nirum --help
    Nirum Compiler 0.1.0

    nirum [OPTIONS] DIR

    Common flags:
      -o --output-dir=DIR   The directory to place object files
      -? --help             Display help message
      -V --version          Print version information
         --numeric-version  Print just the version number


Building
--------

If you already installed [Haskell Platform][5] or [Haskell Stack][6],
you can build the project in the same way to build other Haskell projects.

Using Stack:

    $ stack build

Using vanilla Cabal:

    $ cabal sandbox init
    $ cabal install --only-dependencies
    $ cabal configure
    $ cabal build

[5]: https://www.haskell.org/platform/
[6]: https://www.haskellstack.org/


Etymology
---------

**니름** (IPA: /niɾɯm/; *nireum*) is a sort of telepathy in the fictional world
of [The Bird That Drinks Tears][7] (눈물을 마시는 새 *Nunmureul masineun sae*)
by [Lee Yeongdo][8] (이영도).

[7]: https://en.wikipedia.org/wiki/The_Bird_That_Drinks_Tears
[8]: https://en.wikipedia.org/wiki/Lee_Yeongdo
