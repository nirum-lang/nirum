Nirum changelog
===============

Version 0.2.0
-------------

Still unstable release.  Released on September 26, 2016.

### Language

 -  The `boxed` keyword was renamed to `unboxed`.  [[#65], [#81]]
 -  [Annotations](./docs/annotation.md) became renewed and complete
    so that every type and module now can be annotated.
    [[#40], [#73]]
 -  Docstrings became merely a syntactic sugar of `@docs` annotation.
    [[#53], [#57]]
 -  Fixed a parser bug which had failed to parse spaces right before/after tag
    parentheses.  [[#69], [#71]]
 -  Fixed a parser bug which had referred to a wrong line/column position on
    syntax error message when a trailing semicolon is missing.  [[#64]]

[#40]: https://github.com/spoqa/nirum/issues/40
[#53]: https://github.com/spoqa/nirum/pull/53
[#57]: https://github.com/spoqa/nirum/pull/57
[#64]: https://github.com/spoqa/nirum/pull/64
[#65]: https://github.com/spoqa/nirum/issues/65
[#69]: https://github.com/spoqa/nirum/issues/69
[#71]: https://github.com/spoqa/nirum/pull/71
[#73]: https://github.com/spoqa/nirum/pull/73
[#81]: https://github.com/spoqa/nirum/pull/81

### Python target

 -  Services became to have their own client implementation of a name with
    `_Client` postfix when they are compiled to Python. [[#52]]
 -  Generated types became to have `__hash__()` method so that they are now
    hashable.  [[#75], [#76]]
 -  Fixed a bug that a Python class generated from a parameterless tag had
    been broken.  [[#55], [#66]]

[#52]: https://github.com/spoqa/nirum/pull/52
[#75]: https://github.com/spoqa/nirum/issues/75
[#76]: https://github.com/spoqa/nirum/pull/76
[#66]: https://github.com/spoqa/nirum/pull/66
[#55]: https://github.com/spoqa/nirum/issues/55

### Et cetera

 -  Introduced the official Docker image.  The image repository is located to
    [spoqa/nirum](https://hub.docker.com/r/spoqa/nirum/).
    [[#48] by Minyoung Jeong]

[#48]: https://github.com/spoqa/nirum/pull/48



Version 0.1.0
-------------

Initial and unstable release for a
[demo session at PyCon APAC 2016][pycon-apac-2016].
Released on August 14, 2016.

[pycon-apac-2016]: https://www.pycon.kr/2016apac/program/36
