Nirum
=====

Nirum is a [distributed object][1] framework/compiler for microservices,
built on top of the modern Web server technologies such as RESTful HTTP and
JSON.

WIP.

[1]: https://en.wikipedia.org/wiki/Distributed_object


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
      -o --objectpath=FILE  The directory to place object files
      -? --help             Display help message
      -V --version          Print version information
         --numeric-version  Print just the version number


Etymology
---------

**니름** (IPA: /niɾɯm/; *nireum*) is a sort of telepathy in the fictional world
of [The Bird That Drinks Tears][2] (눈물을 마시는 새 *Nunmureul masineun sae*)
by [Lee Yeongdo][3] (이영도).

[2]: https://en.wikipedia.org/wiki/The_Bird_That_Drinks_Tears
[3]: https://en.wikipedia.org/wiki/Lee_Yeongdo
