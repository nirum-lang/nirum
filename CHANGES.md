Nirum changelog
===============

Version 0.4.0
-------------

To be released.

### Language

 -  Union tags became possible to have `default` keyword.  It's useful
    for migrating a record type to a union type.  [[#13], [#227]]

### Python target

 -  Generated Python packages became to have two [entry points] (a feature
    provided by *setuptools*):
     -  `nirum.modules`: It maps Nirum modules to Python modules.
        Nirum module paths are normalized to avoid underscores and upper letters
        and use hyphens and lower letters instead, e.g., `foo-bar.baz`.
        The table works well with `renames` settings as well.
     -  `nirum.classes`: It maps Nirum types (including services) to Python
        classes.  Nirum type names are qualified and their leading module paths
        are also normalized (the same rule to `nirum.modules` applies here).

 -  The `uri` type became represented as [`basestring`][python-basestring]
    instead of [`unicode`][python-unicode] in Python 2, since URI (unlike IRI)
    is limited to a subset of ASCII character set.

    There's no change to Python 3.

 -  Generated type constructors became to validate field value's range or format
    besides class checks: range checks for `int32`/`int64`, time zone
    (``tzinfo``) awareness check for `datetime`, and basic format check for
    `uri`.

 -  Fixed a bug that generated service methods hadn't checked its arguments
    before its transport sends a payload.  [[#220]]

 -  Fixed a bug that field/parameter names that use a module name of the Python
    standard library cause runtime `TypeError`s (due to name shadowing).
    Under the hood, all generated `import`s are now aliased with a name prefixed
    an underscore.

[#13]: https://github.com/spoqa/nirum/issues/13
[#220]: https://github.com/spoqa/nirum/issues/220
[#227]: https://github.com/spoqa/nirum/pull/227
[entry points]: https://setuptools.readthedocs.io/en/latest/pkg_resources.html#entry-points
[python-basestring]: https://docs.python.org/2/library/functions.html#basestring
[python-unicode]: https://docs.python.org/2/library/functions.html#unicode


Version 0.3.1
-------------

Released on March 1, 2018.

### Python target

 -  Fixed record/union deserializers to ignore unknown fields in data payload.
    Deserializers had raised `KeyError` before.  [[#232]]

[#232]: https://github.com/spoqa/nirum/issues/232


Version 0.3.0
-------------

Released on February 18, 2018.

### Language

 -  [Package](./docs/package.md) is now a new compilation unit of Nirum.
    Every Nirum package needs *package.toml* manifest file.
    [[#83], [#99]]
 -  Since a Nirum package can be compiled to more than one target languages,
    the `nirum` command became to have `-t`/`--target` required parameter.
    [[#106], [#111], [#114]]
 -  Added `-w`/`--watch` mode.  [[#91], [#104], [#218]]
 -  Annotations became able to have multiple arguments and every parameter
    became necessary to having its name (keyword).  [[#178], [#190], [#197]]
 -  Service methods became able to omit their return types.
    [[#179], [#199] by Yang Chun Ung]
 -  Added `@error` annotation to make a type to subclass an exception base
    class (e.g., [`Exception`][python-exception] in Python) when it's compiled
    to OO languages.  [[#38], [#127]]
 -  Union tag docstrings in parentheses became allowed.  [[#153], [#154]]
 -  Fixed a parser bug that a bare identifier (i.e., unquoted identifier) cannot
    start with a reserved keyword, e.g., `types` (`type` is reserved),
    `enumeration` (`enum` is reserved).  [[#184], [#189]]
 -  Fixed a parser bug that `import` names had been disallowing to have
    a trailing comma or newlines.  [[#202]]
 -  Fixed the `nirum` command bug that it had always terminated with exit code
    0 even when it errored.  [[#97], [#108]]

### Docs target

 -  A new target, `docs` is now available.  To generate docs for a Nirum
    package, specify `--target docs` option to the `nirum` command.
    [[#10], [#113], [#116], [#125], [#131], [#152], [#170], [#223]]

### Python target

 -  Now supports Python 2.7 besides Python 3.4 or later.
    [[#50], [#85], [#93], [#117], [nirum-python #22]]
 -  Now requires *nirum-python* 0.6.0 or later.  [[#119], [#141], [#146]]
 -  From now on, in order to compile a Nirum package to Python,
    *package.toml* manifest need [`targets.python`][targets.python] section
    and `targets.python.name` field.  [[#99]]
 -  Added `targets.python.minimum_runtime` option to specify the minimum
    version of *nirum-python* runtime library.  [[#118], [#119]]
 -  Added `targets.python.renames` option to rename module names when they
    are compiled to a Python module.  [[#121]]
 -  More package metadata became configurable.  [[#100]]
     -  `targets.python.description`  [[#174] by Seunghun Lee]
     -  `targets.python.license`  [[#180] by Seunghun Lee]
     -  `targets.python.keywords`  [[#183] by Seunghun Lee]
 -  Added new transport layer.  [[#149], [nirum-python #79], [nirum-python #92]]
     -  Generated constructors of service clients became to take a
        `nirum.transport.Transport` instance.
     -  Followed renamed/moved import paths of the runtime classes
        (e.g., `nirum.rpc.Service` became to `nirum.service.Service`).
     -  The way to avoid name collision between generated types and runtime
        classes is changed.  The runtime library had provided alternative names
        like `service_type` for `Service` and generated imports had been like
        `from nirum.rpc import service_type`, but it's now
        `from nirum.service import Service as service_type`.
 -  Record/union tag fields of an optional type can be omitted when
    the constructor is called.
    [[#70], [#165] by Seunghun Lee]
 -  Generated tag classes became qualified under its union class
    (e.g., `Shape.Rectangle` instead of `Rectangle`).
    Deprecated the old style and it is going to be obsolete in the near future.
    [[#68], [#193]]
 -  Generated service clients became qualified under its service class
    (e.g., `FooService.Client` instead of `FooService_Client`).
    Deprecated the old style and it is going to be obsolete in the near future.
    [[#167], [#222]].
 -  Generated serializers became independent from *nirum-python* runtime
    library.  [[#160], [#201], [#203], [#204]]
 -  Deserializers became to show multiple error messages at a time.
    [[#168], [#224]]
 -  Generated Python classes became having `__nirum_type__` metadata for RTTI.
    [[nirum-python #34], [#192]]
 -  Generated service classes became having `__nirum_method_annotations__`
    metadata for processing annotations.  [[#194]]
 -  Docstrings in a Nirum schema became to generate corresponding Python
    docstrings.  [[#102], [#128]]
 -  Sets, lists, and maps became compiled to immutable data structures
    so that thay are easily hashable.  [[nirum-python #49], [#123]]
 -  Fixed a bug that implicit ancestor packages hadn't been generated even
    if they have submodules.  [[#92], [#105]]
 -  Fixed a bug that a generated Python code had raised
    [`NameError`][python-name-error] when a referring type is above than
    a referred type.  [[#138], [nirum-python #88], [#146]]
 -  Fixed a bug that a generated Python `enum` code had became broken
    when an enum type has a member named `mro`.  [[#185], [#188]]

### Et cetera

 -  The officially distributed executable binaries for Linux became
    independent from [glibc]; instead statically linked to [musl].  [#216]
 -  The Docker image now has `nirum` command in `PATH`.  [[#155]]
 -  The Docker image became based and built on [Alpine Linux][] so that
    the image is now much lighter.

[#10]: https://github.com/spoqa/nirum/issues/10
[#38]: https://github.com/spoqa/nirum/issues/38
[#50]: https://github.com/spoqa/nirum/issues/50
[#68]: https://github.com/spoqa/nirum/issues/68
[#70]: https://github.com/spoqa/nirum/issues/70
[#83]: https://github.com/spoqa/nirum/pull/83
[#85]: https://github.com/spoqa/nirum/pull/85
[#91]: https://github.com/spoqa/nirum/issues/91
[#92]: https://github.com/spoqa/nirum/issues/92
[#93]: https://github.com/spoqa/nirum/issues/93
[#99]: https://github.com/spoqa/nirum/pull/99
[#97]: https://github.com/spoqa/nirum/issues/97
[#100]: https://github.com/spoqa/nirum/issues/100
[#102]: https://github.com/spoqa/nirum/issues/102
[#104]: https://github.com/spoqa/nirum/pull/104
[#105]: https://github.com/spoqa/nirum/pull/105
[#106]: https://github.com/spoqa/nirum/pull/106
[#108]: https://github.com/spoqa/nirum/pull/108
[#111]: https://github.com/spoqa/nirum/pull/111
[#113]: https://github.com/spoqa/nirum/pull/113
[#114]: https://github.com/spoqa/nirum/pull/114
[#116]: https://github.com/spoqa/nirum/pull/116
[#117]: https://github.com/spoqa/nirum/pull/117
[#118]: https://github.com/spoqa/nirum/issues/118
[#119]: https://github.com/spoqa/nirum/pull/119
[#121]: https://github.com/spoqa/nirum/pull/121
[#123]: https://github.com/spoqa/nirum/pull/123
[#128]: https://github.com/spoqa/nirum/pull/128
[#125]: https://github.com/spoqa/nirum/issues/125
[#127]: https://github.com/spoqa/nirum/pull/127
[#131]: https://github.com/spoqa/nirum/pull/131
[#138]: https://github.com/spoqa/nirum/issues/138
[#141]: https://github.com/spoqa/nirum/pull/141
[#146]: https://github.com/spoqa/nirum/pull/146
[#149]: https://github.com/spoqa/nirum/pull/149
[#152]: https://github.com/spoqa/nirum/pull/152
[#153]: https://github.com/spoqa/nirum/issues/153
[#154]: https://github.com/spoqa/nirum/pull/154
[#155]: https://github.com/spoqa/nirum/pull/155
[#160]: https://github.com/spoqa/nirum/issues/160
[#165]: https://github.com/spoqa/nirum/pull/165
[#167]: https://github.com/spoqa/nirum/pull/167
[#168]: https://github.com/spoqa/nirum/issues/168
[#170]: https://github.com/spoqa/nirum/pull/170
[#174]: https://github.com/spoqa/nirum/pull/174
[#178]: https://github.com/spoqa/nirum/issues/178
[#179]: https://github.com/spoqa/nirum/issues/179
[#180]: https://github.com/spoqa/nirum/pull/180
[#183]: https://github.com/spoqa/nirum/pull/183
[#184]: https://github.com/spoqa/nirum/issues/184
[#185]: https://github.com/spoqa/nirum/issues/185
[#188]: https://github.com/spoqa/nirum/pull/188
[#189]: https://github.com/spoqa/nirum/pull/189
[#190]: https://github.com/spoqa/nirum/pull/190
[#192]: https://github.com/spoqa/nirum/pull/192
[#193]: https://github.com/spoqa/nirum/pull/193
[#194]: https://github.com/spoqa/nirum/pull/194
[#197]: https://github.com/spoqa/nirum/pull/197
[#199]: https://github.com/spoqa/nirum/pull/199
[#201]: https://github.com/spoqa/nirum/pull/201
[#202]: https://github.com/spoqa/nirum/pull/202
[#203]: https://github.com/spoqa/nirum/pull/203
[#204]: https://github.com/spoqa/nirum/pull/204
[#216]: https://github.com/spoqa/nirum/issues/216
[#218]: https://github.com/spoqa/nirum/issues/218
[#222]: https://github.com/spoqa/nirum/pull/222
[#223]: https://github.com/spoqa/nirum/pull/223
[#224]: https://github.com/spoqa/nirum/pull/224
[nirum-python #22]: https://github.com/spoqa/nirum-python/issues/22
[nirum-python #34]: https://github.com/spoqa/nirum-python/issues/34
[nirum-python #49]: https://github.com/spoqa/nirum-python/issues/49
[nirum-python #79]: https://github.com/spoqa/nirum-python/issues/79
[nirum-python #88]: https://github.com/spoqa/nirum-python/pull/88
[nirum-python #92]: https://github.com/spoqa/nirum-python/pull/92
[python-exception]: https://docs.python.org/3/library/exceptions.html#Exception
[targets.python]: ./target/python.md
[python-name-error]: https://docs.python.org/3/library/exceptions.html#NameError
[glibc]: https://www.gnu.org/software/libc/
[musl]: https://www.musl-libc.org/
[Alpine Linux]: https://alpinelinux.org/


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
