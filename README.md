Nirum
=====

[![The latest release on GitHub][release-svg]][release]
[![Docker automated build][docker-svg]][docker]
[![Build status on Linux and macOS (Travis CI)][ci-svg]][ci]
[![Build status on Windows (AppVeyor)][ciw-svg]][ciw]
[![Test coverage (codecov)][cov-svg]][cov]
[![Total lines of code][loc]][repo]
[![Gitter][chat-svg]][chat]

[release-svg]: https://img.shields.io/github/release/spoqa/nirum/all.svg
[release]: https://github.com/spoqa/nirum/releases
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
    $ nirum -t python -o out/ examples/

For more infomration, use `--help` option:

    $ nirum --help
    Nirum: The IDL compiler and RPC/distributed object framework

    Usage: nirum [-v|--version] (-o|--output-dir DIR) (-t|--target TARGET) DIR
      Nirum compiler 0.3.0

    Available options:
      -h,--help                Show this help text
      -v,--version             Show version
      -o,--output-dir DIR      Output directory
      -t,--target TARGET       Target language name. Available: docs, python
      DIR                      Package directory

There is a [step-by-step tutorial](./docs/tutorial.md) as well.


Building
--------

If you already installed [Haskell Stack][5], you can build the project
in the same way to build other Haskell softwares:

    $ stack build

You can run the test suite of Nirum:

    $ stack test  # unit test for compiler
    $ ./lint.sh   # style lint

For details, please read the [contribution guide](./CONTRIBUTING.md).

[5]: https://www.haskellstack.org/


Related projects & tools
------------------------

See also the [list of Nirum-related projects][7] on GitHub.  Have you kicked off
a new project related to Nirum?  Please add *nirum* [topic][8] to your project
on GitHub!

### Language runtimes

 -   [nirum-python](https://github.com/spoqa/nirum-python): The official Python
     runtime library for Nirum.
     -   [nirim-python-http](https://github.com/spoqa/nirum-python-http):
         Nirum HTTP transport for Python.
     -   [nirum-python-wsgi](https://github.com/spoqa/nirum-python-wsgi):
         Adapt Nirum services to WSGI apps.

### Editor supports

 -   [nirum.tmbundle](https://github.com/spoqa/nirum.tmbundle): TextMate bundle
     for Nirum.  Also can be used by IntelliJ IDEA (or any other JetBrain's
     IDEs, e.g., PyCharm, WebStorm) through [TextMate bundles support][9].
 -   [nirum.vim](https://github.com/spoqa/nirum.vim): Nirum syntax highlighter for
     Vim/Neovim.
 -   [sublime-nirum](https://github.com/spoqa/sublime-nirum): Nirum package for
     Sublime Text 3.

[7]: https://github.com/search?q=topic:nirum+fork:false
[8]: https://github.com/blog/2309-introducing-topics
[9]: https://github.com/spoqa/nirum.tmbundle#installation-intellij-idea-pycharm-etc


Etymology
---------

**니름** (IPA: /niɾɯm/; *nireum*) is a sort of telepathy in the fictional world
of [The Bird That Drinks Tears][9] (눈물을 마시는 새 *Nunmureul masineun sae*)
by [Lee Yeongdo][10] (이영도).

[9]: https://en.wikipedia.org/wiki/The_Bird_That_Drinks_Tears
[10]: https://en.wikipedia.org/wiki/Lee_Yeongdo
