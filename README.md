Nirum
=====

Nirum is a [distributed object][1] framework/compiler for microservices,
built on top of the modern Web server technologies such as RESTful HTTP and
JSON.

WIP.

[1]: https://en.wikipedia.org/wiki/Distributed_object


Getting Started
---------------

In order to compile `examples/shapes.nrm` file to python in `out` directory:

    $ nirum --python -d=./out ./examples/shapes.nrm

For more infomration, type `--help`:

    $ nirum --help
    Nirum Compiler CLI

    nirum [OPTIONS] FILE

    Common flags:
      -p --python            python
      -d --destination=DEST  Write generated file to DEST
      -? --help              Display help message
      -V --version           Print version information


Etymology
---------

**니름** (IPA: /niɾɯm/; *nireum*) is a sort of telepathy in the fictional world
of [The Bird That Drinks Tears][2] (눈물을 마시는 새 *Nunmureul masineun sae*)
by [Lee Yeongdo][3] (이영도).

[2]: https://en.wikipedia.org/wiki/The_Bird_That_Drinks_Tears
[3]: https://en.wikipedia.org/wiki/Lee_Yeongdo
