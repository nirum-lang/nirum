Nirum
=====

[![The latest release on GitHub][release-svg]][releases]
[![Docker automated build][docker-svg]][docker]
[![Build status on Linux and macOS (Travis CI)][ci-svg]][ci]
[![Build status on Windows (AppVeyor)][ciw-svg]][ciw]
[![Test coverage (codecov)][cov-svg]][cov]
[![Total lines of code][loc]][repo]
[![Gitter][chat-svg]][chat]

[release-svg]: https://img.shields.io/github/release/spoqa/nirum/all.svg
[releases]: https://github.com/spoqa/nirum/releases
[docker]: https://hub.docker.com/r/spoqa/nirum/
[docker-svg]: https://img.shields.io/docker/automated/spoqa/nirum.svg
[ci-svg]: https://travis-ci.org/spoqa/nirum.svg?branch=master
[ci]: https://travis-ci.org/spoqa/nirum
[ciw-svg]: https://ci.appveyor.com/api/projects/status/jf9bsrnalcb1xrp0?svg=true
[ciw]: https://ci.appveyor.com/project/dahlia/nirum-k5n5y
[cov-svg]: https://codecov.io/gh/spoqa/nirum/branch/master/graph/badge.svg
[cov]: https://codecov.io/gh/spoqa/nirum
[loc]: https://tokei.rs/b1/github/spoqa/nirum
[repo]: https://github.com/spoqa/nirum
[chat-svg]: https://badges.gitter.im/spoqa/nirum.svg
[chat]: https://gitter.im/spoqa/nirum?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge

Nirum is an [IDL] compiler and [RPC]/[distributed object] framework
for [microservices], built on top of the modern Web server technologies
such as RESTful HTTP and JSON.

You can find how the language looks like from source codes in the `examples/`
directory.

**Note that its design is highly unstable and could be changed.**
Also the feature set is incomplete yet.

[IDL]: https://en.wikipedia.org/wiki/Interface_description_language
[RPC]: https://en.wikipedia.org/wiki/Remote_procedure_call
[distributed object]: https://en.wikipedia.org/wiki/Distributed_object
[microservices]: https://en.wikipedia.org/wiki/Microservices


Installation
------------

The Nirum compiler works on the most major platforms like Linux, macOS, and
Windows.  We provide the prebuilt executable binaries for these three platforms.


### Released builds

You can download a executable binary for Linux (x86_64), macOS (x86_64), or
Windows (x64 or x86) from the [latest release note][latest-release].
You should give it appropriate permissions (e.g., `+x`) depending on your
platform.

If you look for a previous release, see the [list of all releases][releases].

We provide official Docker images as well.  You can check the list of
[released tags][docker-tags]. *Note that `latest` is for a nightly build which
is unstable (see below).*

[latest-release]: https://github.com/spoqa/nirum/releases/latest
[docker-tags]: https://hub.docker.com/r/spoqa/nirum/tags/


### Nightly builds

Since Nirum is still changing by leaps and bounds, you could want to try
its bleeding edge.  We provide nightly builds for this purpose.

- [Linux (x86_64)](https://nightly-builds.nirum.org/travis-builds/nirum-linux-x86_64)
- [macOS (x86_64)](https://nightly-builds.nirum.org/travis-builds/nirum-darwin-x86_64)
- [Windows (x64)](https://ci.appveyor.com/api/projects/dahlia/nirum-k5n5y/artifacts/nirum-win-x64.exe?job=Platform%3A%20x64&branch=master)
- [Windows (x86)](https://ci.appveyor.com/api/projects/dahlia/nirum-k5n5y/artifacts/nirum-win-x86.exe?job=Platform%3A%20x86&branch=master)
- [Docker (`spoqa/nirum:latest`)][docker]

Although we call it "nightly build," technically it is not built every night,
but done every merge commit.


Getting started
---------------

In order to compile a Nirum package (`examples/`) to a Python package:

    $ mkdir out/  # directory to place generated Python files
    $ nirum -t python -o out/ examples/

For more infomration, use `--help` option:

    $ nirum --help
    Nirum: The IDL compiler and RPC/distributed object framework

    Usage: nirum [-v|--version] (-o|--output-dir DIR) (-t|--target TARGET) DIR
      Nirum compiler 0.3.3

    Available options:
      -h,--help                Show this help text
      -v,--version             Show version
      -o,--output-dir DIR      Output directory
      -t,--target TARGET       Target language name. Available: docs, python
      DIR                      Package directory

There is a [step-by-step tutorial](./docs/tutorial.md) as well.


Building
--------

If you already installed [Haskell Stack], you can build the project
in the same way to build other Haskell softwares:

    $ stack build

You can run the test suite of Nirum:

    $ stack test  # unit test for compiler
    $ ./lint.sh   # style lint

For details, please read the [contribution guide](./CONTRIBUTING.md).

[Haskell Stack]: https://www.haskellstack.org/


Related projects & tools
------------------------

See also the [list of Nirum-related projects][related-projects] on GitHub.
Have you kicked off a new project related to Nirum?  Please add *nirum*
[topic][github-topic] to your project on GitHub!

### Language runtimes

 -   [nirum-python](https://github.com/spoqa/nirum-python): The official Python
     runtime library for Nirum.
     -   [nirum-python-http](https://github.com/spoqa/nirum-python-http):
         Nirum HTTP transport for Python.
     -   [nirum-python-wsgi](https://github.com/spoqa/nirum-python-wsgi):
         Adapt Nirum services to WSGI apps.

### Editor supports

 -   [nirum.tmbundle](https://github.com/spoqa/nirum.tmbundle): TextMate bundle
     for Nirum.  Also can be used by IntelliJ IDEA (or any other JetBrain's
     IDEs, e.g., PyCharm, WebStorm) through [TextMate bundles support].
 -   [nirum.vim](https://github.com/spoqa/nirum.vim): Nirum syntax highlighter for
     Vim/Neovim.
 -   [sublime-nirum](https://github.com/spoqa/sublime-nirum): Nirum package for
     Sublime Text 3.

[related-projects]: https://github.com/search?q=topic:nirum+fork:false
[github-topic]: https://github.com/blog/2309-introducing-topics
[TextMate bundles support]: https://github.com/spoqa/nirum.tmbundle#installation-intellij-idea-pycharm-etc


Etymology
---------

**니름** (IPA: /niɾɯm/; *nireum*) is a sort of telepathy in the fictional world
of [The Bird That Drinks Tears] (눈물을 마시는 새 *Nunmureul masineun sae*)
by [Lee Yeongdo] (이영도).

[The Bird That Drinks Tears]: https://en.wikipedia.org/wiki/The_Bird_That_Drinks_Tears
[Lee Yeongdo]: https://en.wikipedia.org/wiki/Lee_Yeongdo
