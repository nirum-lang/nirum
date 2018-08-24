Nirum changelog
===============

Version 0.6.0
-------------

To be released.


Version 0.5.0
-------------

Released on August 24, 2018.

### Language

 -  The `uri` type has completly gone; use `url` instead.
    [[#126], [#281] by Jonghun Park]
 -  Added [`@numeric-constraints`](docs/annotation.md#numeric-constraints)
    annotation to constraint the range of unboxed types' values.
    [[#206], [#271] by Seunghun Lee]

### Docs target

 -  Docs target became to support manual pages besides reference docs extracted
    from Nirum source codes.  It scans all CommonMark (i.e., _*.md_) files and
    transforms them to HTML pages.
 -  [CommonMark] in docstrings became to support [table syntax extension].
 -  [CommonMark] in docstrings became to have a limited subset of
    [special attributes extension].  It's only allowed to heading elements and
    only anchor identifiers are supported (e.g., `{#header-id}`).
 -  `style`, `header`, and `footer` options were added.  These options purpose
    to customize the look and feel of the result pages.
 -  Fixed an incorrect processing of [CommonMark] thight list items: it had
    crashed when a thight list item contains blocks other than paragraphs.
 -  Package's version became shown on the generated docs.
    [[#297], [#304] by Jeong Ukjae]
 -  Added `opengraphs` option for [OpenGraph] objects on docs.
    [[#283], [#305] by GyuYong Jung]
 -  Added syntax highlighting including Nirum.
    [[#310], [#311], [#313], [#324] by MinJune Kim]
 -  Added anchor links to types.
    [[#225], [#321] by Park Hyunwoo]

### Python target

 -  Python 3.7 support. [[#298], [#300] by Alan D.J. Synn]
 -  Fixed a bug that an uncaught `NameError` raises (instead of following
    the standardized way to handle validation errors through `on_error`)
    when a serialized value of a mapping lacks `"key"` or `"value"`
    field and a type/union tag/service named `key-error` is defined in the same
    module at the same time.  [[#318] by Dongwook Kim]
 -  Fixed a bug that an uncaught `NameError` raises (instead of following
    the standardized way to handle validation errors through `on_error`)
    when a serialized value of a `bigint` is not an integral decimal string
    and a type/union tag/service named `value-error` is defined in the same
    module at the same time.  [[#318] by Dongwook Kim]

### Et cetera

 -  Dropped 32-bit Windows support.

[#126]: https://github.com/nirum-lang/nirum/issues/126
[#206]: https://github.com/nirum-lang/nirum/issues/206
[#225]: https://github.com/nirum-lang/nirum/issues/225
[#271]: https://github.com/nirum-lang/nirum/pull/271
[#281]: https://github.com/nirum-lang/nirum/pull/281
[#283]: https://github.com/spoqa/nirum/pull/283
[#297]: https://github.com/nirum-lang/nirum/issues/297
[#298]: https://github.com/nirum-lang/nirum/issues/298
[#300]: https://github.com/nirum-lang/nirum/pull/300
[#304]: https://github.com/nirum-lang/nirum/pull/304
[#305]: https://github.com/nirum-lang/nirum/pull/305
[#310]: https://github.com/nirum-lang/nirum/issues/310
[#311]: https://github.com/nirum-lang/nirum/pull/311
[#313]: https://github.com/nirum-lang/nirum/issues/313
[#318]: https://github.com/nirum-lang/nirum/pull/318
[#321]: https://github.com/nirum-lang/nirum/pull/321
[#324]: https://github.com/nirum-lang/nirum/pull/324
[CommonMark]: http://commonmark.org/
[table syntax extension]: https://github.github.com/gfm/#tables-extension-
[special attributes extension]: https://michelf.ca/projects/php-markdown/extra/#spe-attr
[OpenGraph]: http://ogp.me/


Version 0.4.2
-------------

Released on Aug 24, 2018.

### Language

 -  Fixed a bug that *package.toml* file had been failed to load when the system
    locale does not use UTF-8. It's now independent from the system locale
    settings.
    [[#290], [#322] by Seunghun Lee]

### Et cetera

 -  The official Docker image repository was moved to
    [nirumlang/nirum](https://hub.docker.com/r/nirumlang/nirum/)
    (from [spoqa/nirum](https://hub.docker.com/r/spoqa/nirum/)).

[#290]: https://github.com/nirum-lang/nirum/issues/290
[#322]: https://github.com/nirum-lang/nirum/pull/322


Version 0.4.1
-------------

Released on June 8, 2018.

### Et cetera

 -  The official Docker images became to have netbase pacakage. See the
    [related issue on Haskell Stack][commercialhaskell/stack#2372] as well.

[commercialhaskell/stack#2372]: https://github.com/commercialhaskell/stack/issues/2372


Version 0.4.0
-------------

Released on May 25, 2018.

### Language

 -  Union tags became possible to have `default` keyword.  It's useful
    for migrating a record type to a union type.  [[#13], [#227]]

 -  Enum members and union tags became disallowed to shadow an other type name
    in a module.  It's because in some target languages we compile both types
    and members/tags into objects that share the same namespace.  In Python,
    both types and union tags are compiled into classes.

    For example, the following definitions had been allowed, but now became
    disallowed:

    ~~~~~~~~ nirum
    unboxed foo (text);
    //      ^~~
    enum bar = foo | baz;
    //         ^~~
    ~~~~~~~~

    ~~~~~~~~ nirum
    record foo (text a);
    //     ^~~
    union bar = foo (text b) | baz (text c);
    //          ^~~
    ~~~~~~~~

    This rule is applied even between a member/tag and its belonging enum/union
    type as well.  The following definitions are disallowed:

    ~~~~~~~~ nirum
    enum foo = foo | bar;
         ^~~   ^~~
    ~~~~~~~~

    ~~~~~~~~ nirum
    union bar = foo (text a) | bar (text b);
          ^~~                  ^~~
    ~~~~~~~~

    If you already have used the same name for a type and a tag, you can avoid
    breaking backward compatibility of serialization by specifying a different
    behind name.  For example, if you've had a definition like:

    ~~~~~~~~ nirum
    enum foo = foo | bar;
    //   ^~~   ^~~
    ~~~~~~~~

    It can be changed like the following:

    ~~~~~~~~ nirum
    enum foo = foo-renamed/foo | bar;
    //         ^~~~~~~~~~~ ^~~
    ~~~~~~~~

    [[#254], [#255]]

 -  Enum members and union tags became disallowed to shadow other enum members
    and union tags even if they belong to an other type.  It's because in some
    target language we compile them into objects that share the same namespace,
    regardless of their belonging type.

    For example, the following definitions had been allowed, but now became
    disallowed:

    ~~~~~~~~ nirum
    enum foo = bar | baz;
    //         ^~~
    enum qux = quux | bar;
    //                ^~~
    ~~~~~~~~

    ~~~~~~~~ nirum
    union foo = bar (text a) | baz (text b);
    //          ^~~
    enum qux = quux | bar;
    //                ^~~
    ~~~~~~~~

    ~~~~~~~~ nirum
    union foo = bar (text a) | baz (text b);
    //                         ^~~
    union qux = quux (text c) | baz (text d);
    //                          ^~~
    ~~~~~~~~

    If you already have used the same name for members/tags of different
    enum/union types, you can avoid breaking backward compatibility of
    serialization by specifying a different behind name.
    For example, if you've had a definition like:

    ~~~~~~~~ nirum
    enum foo = bar | baz;
    //         ^~~
    union qux = bar (text a) | quux (text b);
    //          ^~~
    ~~~~~~~~

    It can be changed like the following:

    ~~~~~~~~ nirum
    enum foo = bar-renamed/bar | baz;
    //         ^~~~~~~~~~~ ^~~
    union qux = bar (text a) | quux (text b);
    ~~~~~~~~

    [[#254], [#255]]

 -  Fixed a compiler bug that an error message on name duplicates had referred
    to a wrong line/column number.  [[#255]]

 -  Added aliased import.  It's handy to avoid a name shadowing.
    [[#217], [#258]]

    ~~~~~~~~ nirum
    import iso (country as iso-country);
    import types (country);
    ~~~~~~~~

 -  Added support for integer type annotation argument.  [[#178], [#267]]

    ~~~~~~~~ nirum
    service foo-service (
        @bar(baz=1)
        int32 qux (int32 quux),
    );
    ~~~~~~~~

 -  Deprecated `uri` and `url` type is added.  [[#126], [#277] by Jonghun Park]

### Docs target

 -  A new required configuration `targets.docs.title` was added.
    It's rendered in generated HTML documents' `<title>` element.
    [[#253]]

 -  Docs now have a sidebar which contains table of contents.  [[#257]]

 -  Fixed a bug that a module-level docs hadn't been rendered.
    Now it's shown in the right below the module name.  [[#259]]

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

 -  Generated deserializers became independent from *nirum-python* runtime
    library.  [[#160], [#272]]

     -  Error messages made during deserialization became more standardized.
        Every error now consists of two fields: one represents a path to the
        value having an error, and other one is a human-readable message.

        For example, suppose there's types like:

        ~~~~~~~~ nirum
        record user (dob date);
        record user-group ([user] users);
        ~~~~~~~~

        and the following payload for `user-group` type is given:

        ~~~~~~~~ json
        [
          {"_type": "user", "dob": "2000-06-30"},
          {"_type": "user", "dob": "2000-13-32"}
        ]
        ~~~~~~~~

        An error is reported with a path like `[1].dob`.

     -  Added an optional `on_error` callback parameter to generated
        `__nirum_deserialize__()` methods.

        It has to be a callable which has two parameters (and no return value).
        An `on_error` callback is called every time any validation/parsing error
        happens during deserialization, and it can be called multiple times
        at a single call of `__nirum_deserialize__()`.

        The callback function's first parameter takes a string referring
        a path to the value having an error (e.g., `'.users[0].dob'`).
        The second parameter takes an error message string (e.g.,
        `'Expected a string of RFC 3339 date, but the date is invalid.'`).

        When it's omitted or `None`, a `ValueError` is raised with a multiline
        message that consists of all error messages made during deserialization,
        as it has been.

 -  Fixed a bug that record/union deserializers refuses payloads without
    `"_type"` field.  It is only for humans and can be omitted according to
    the [specification](./docs/serialization.md).

 -  All integral types (`int32`, `int64`, and `bigint`) became represented
    as [`numbers.Integral`][python2-numbers-integral] instead of
    [`int`][python2-int].

    There's no change to Python 3.

 -  The `uri` type became represented as [`basestring`][python2-basestring]
    instead of [`unicode`][python2-unicode] in Python 2, since URI (unlike IRI)
    is limited to a subset of ASCII character set.

    There's no change to Python 3.

 -  Generated type constructors became to validate field value's range or format
    besides class checks: range checks for `int32`/`int64`, time zone
    (``tzinfo``) awareness check for `datetime`, and basic format check for
    `uri`.

 -  Generated service methods became to have its own serialization and
    deserialization functions.  Each method object now has these attributes:

     -  `__nirum_serialize_arguments__` takes the same keywords to the method
        parameters and serialize them into a mapping object (which can be
        directly translated to a JSON object).  It can raise a `TypeError` or
        `ValueError` if any invalid values are passed.

     -  `__nirum_deserialize_arguments__` takes a mapping object returned by
        `json.load()`/`json.loads()` (and an optional `on_error` callable)
        and deserialize it into a mapping object of pairs from parameter's
        facial name string to its corresponding Python object.

     -  `__nirum_argument_serializers__` is a mapping object that keys are
        a string of method's parameter facial name and values are its
        serializer.

        A serializer function takes an argument value and returns
        its corresponding value which can be passed to
        `json.dump()`/`json.dumps()`.  It can raise a `TypeError` or
        `ValueError` if an argument is invalid.

     -  `__nirum_argument_deserializers__` is a mapping object that keys are
        a string of method's parameter behind name and values are its
        deserializer.

        A deserializer function takes an argument value preprocessed by
        `json.load()`/`json.loads()` with an optional `on_error` callback,
        and returns its corresponding Python object.

     -  `__nirum_serialize_result__` takes a method's return value and serialize
        it into a corresponding value which can be passed to
        `json.dump()`/`json.dumps()`.

        If the given value does not match to method's return type it raises
        `TypeError`, or `ValueError` if the value is invalid.

        This attribute is `None` if the method has no return type.

     -  `__nirum_deserialize_result__` takes a result value preprocessed by
        `json.load()`/`json.loads()` with an optional `on_error` callback,
        and deserialize it into its corresponding Python object.

        This attribute is `None` if the method has no return type.

     -  `__nirum_serialize_error__` takes a method's error object and serialize
        it into a corresponding value which can be passed to
        `json.dump()`/`json.dumps()`.

        If the given error object does not match to method's return type
        it raises `TypeError`, or `ValueError` if the error object is invalid.

        This attribute is `None` if the method has no error type.

     -  `__nirum_deserialize_error__` takes an error value preprocessed by
        `json.load()`/`json.loads()` with an optional `on_error` callback,
        and deserialize it into its corresponding Python object.

        This attribute is `None` if the method has no error type.

 -  Removed `__nirum_get_inner_type__()` class methods from generated unboxed
    type classes.

 -  Removed `__nirum_record_behind_name__` static fields and
    `__nirum_field_types__()` class methods from generated record type classes.

 -  Removed `__nirum_tag_names__`, `__nirum_union_behind_name__`, and
    `__nirum_field_names__` static fields from generated union type classes.

 -  Removed `__nirum_tag_types__` static fields from generated union tag
    classes.

 -  Removed `__nirum_schema_version__` static field from generated service
    classes.

 -  Fixed a bug that generated service methods hadn't checked its arguments
    before its transport sends a payload.  [[#220]]

 -  Fixed a bug that field/parameter names that use a module name of the Python
    standard library cause runtime `TypeError`s (due to name shadowing).
    Under the hood, all generated `import`s are now aliased with a name prefixed
    an underscore.

 -  Added Python classifier metadata field.  [[#100], [#269]]

### Et cetera

 -  The officially distributed executable binaries for Linux became
    dependent on [glibc] again.
 -  The official Docker images became based on Debian ([minideb]) instead of
    Alpine Linux.  It's because Alpine Linux doesn't provide GHC 8.2 as of
    March 2018.

[#13]: https://github.com/nirum-lang/nirum/issues/13
[#100]: https://github.com/nirum-lang/nirum/issues/100
[#126]: https://github.com/nirum-lang/nirum/issues/126
[#178]: https://github.com/nirum-lang/nirum/issues/178
[#217]: https://github.com/nirum-lang/nirum/issues/217
[#220]: https://github.com/nirum-lang/nirum/issues/220
[#227]: https://github.com/nirum-lang/nirum/pull/227
[#253]: https://github.com/nirum-lang/nirum/pull/253
[#254]: https://github.com/nirum-lang/nirum/pull/254
[#255]: https://github.com/nirum-lang/nirum/pull/255
[#257]: https://github.com/nirum-lang/nirum/pull/257
[#258]: https://github.com/nirum-lang/nirum/pull/258
[#259]: https://github.com/nirum-lang/nirum/pull/259
[#267]: https://github.com/nirum-lang/nirum/pull/267
[#269]: https://github.com/nirum-lang/nirum/pull/269
[#272]: https://github.com/nirum-lang/nirum/pull/272
[#277]: https://github.com/nirum-lang/nirum/pull/277
[entry points]: https://setuptools.readthedocs.io/en/latest/pkg_resources.html#entry-points
[python2-numbers-integral]: https://docs.python.org/2/library/numbers.html#numbers.Integral
[python2-int]: https://docs.python.org/2/library/functions.html#int
[python2-basestring]: https://docs.python.org/2/library/functions.html#basestring
[python2-unicode]: https://docs.python.org/2/library/functions.html#unicode
[glibc]: https://www.gnu.org/software/libc/
[minideb]: https://hub.docker.com/r/bitnami/minideb/


Version 0.3.3
-------------

Released on March 15, 2018.

### Et cetera

 -  The official Docker images became to have CA certificates.


Version 0.3.2
-------------

Released on March 15, 2018.

### Et cetera

 -  Fixed a broken build of the official Docker images.


Version 0.3.1
-------------

Released on March 1, 2018.

### Python target

 -  Fixed record/union deserializers to ignore unknown fields in data payload.
    Deserializers had raised `KeyError` before.  [[#232]]

[#232]: https://github.com/nirum-lang/nirum/issues/232


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

[#10]: https://github.com/nirum-lang/nirum/issues/10
[#38]: https://github.com/nirum-lang/nirum/issues/38
[#50]: https://github.com/nirum-lang/nirum/issues/50
[#68]: https://github.com/nirum-lang/nirum/issues/68
[#70]: https://github.com/nirum-lang/nirum/issues/70
[#83]: https://github.com/nirum-lang/nirum/pull/83
[#85]: https://github.com/nirum-lang/nirum/pull/85
[#91]: https://github.com/nirum-lang/nirum/issues/91
[#92]: https://github.com/nirum-lang/nirum/issues/92
[#93]: https://github.com/nirum-lang/nirum/issues/93
[#99]: https://github.com/nirum-lang/nirum/pull/99
[#97]: https://github.com/nirum-lang/nirum/issues/97
[#100]: https://github.com/nirum-lang/nirum/issues/100
[#102]: https://github.com/nirum-lang/nirum/issues/102
[#104]: https://github.com/nirum-lang/nirum/pull/104
[#105]: https://github.com/nirum-lang/nirum/pull/105
[#106]: https://github.com/nirum-lang/nirum/pull/106
[#108]: https://github.com/nirum-lang/nirum/pull/108
[#111]: https://github.com/nirum-lang/nirum/pull/111
[#113]: https://github.com/nirum-lang/nirum/pull/113
[#114]: https://github.com/nirum-lang/nirum/pull/114
[#116]: https://github.com/nirum-lang/nirum/pull/116
[#117]: https://github.com/nirum-lang/nirum/pull/117
[#118]: https://github.com/nirum-lang/nirum/issues/118
[#119]: https://github.com/nirum-lang/nirum/pull/119
[#121]: https://github.com/nirum-lang/nirum/pull/121
[#123]: https://github.com/nirum-lang/nirum/pull/123
[#128]: https://github.com/nirum-lang/nirum/pull/128
[#125]: https://github.com/nirum-lang/nirum/issues/125
[#127]: https://github.com/nirum-lang/nirum/pull/127
[#131]: https://github.com/nirum-lang/nirum/pull/131
[#138]: https://github.com/nirum-lang/nirum/issues/138
[#141]: https://github.com/nirum-lang/nirum/pull/141
[#146]: https://github.com/nirum-lang/nirum/pull/146
[#149]: https://github.com/nirum-lang/nirum/pull/149
[#152]: https://github.com/nirum-lang/nirum/pull/152
[#153]: https://github.com/nirum-lang/nirum/issues/153
[#154]: https://github.com/nirum-lang/nirum/pull/154
[#155]: https://github.com/nirum-lang/nirum/pull/155
[#160]: https://github.com/nirum-lang/nirum/issues/160
[#165]: https://github.com/nirum-lang/nirum/pull/165
[#167]: https://github.com/nirum-lang/nirum/pull/167
[#168]: https://github.com/nirum-lang/nirum/issues/168
[#170]: https://github.com/nirum-lang/nirum/pull/170
[#174]: https://github.com/nirum-lang/nirum/pull/174
[#178]: https://github.com/nirum-lang/nirum/issues/178
[#179]: https://github.com/nirum-lang/nirum/issues/179
[#180]: https://github.com/nirum-lang/nirum/pull/180
[#183]: https://github.com/nirum-lang/nirum/pull/183
[#184]: https://github.com/nirum-lang/nirum/issues/184
[#185]: https://github.com/nirum-lang/nirum/issues/185
[#188]: https://github.com/nirum-lang/nirum/pull/188
[#189]: https://github.com/nirum-lang/nirum/pull/189
[#190]: https://github.com/nirum-lang/nirum/pull/190
[#192]: https://github.com/nirum-lang/nirum/pull/192
[#193]: https://github.com/nirum-lang/nirum/pull/193
[#194]: https://github.com/nirum-lang/nirum/pull/194
[#197]: https://github.com/nirum-lang/nirum/pull/197
[#199]: https://github.com/nirum-lang/nirum/pull/199
[#201]: https://github.com/nirum-lang/nirum/pull/201
[#202]: https://github.com/nirum-lang/nirum/pull/202
[#203]: https://github.com/nirum-lang/nirum/pull/203
[#204]: https://github.com/nirum-lang/nirum/pull/204
[#216]: https://github.com/nirum-lang/nirum/issues/216
[#218]: https://github.com/nirum-lang/nirum/issues/218
[#222]: https://github.com/nirum-lang/nirum/pull/222
[#223]: https://github.com/nirum-lang/nirum/pull/223
[#224]: https://github.com/nirum-lang/nirum/pull/224
[nirum-python #22]: https://github.com/nirum-lang/nirum-python/issues/22
[nirum-python #34]: https://github.com/nirum-lang/nirum-python/issues/34
[nirum-python #49]: https://github.com/nirum-lang/nirum-python/issues/49
[nirum-python #79]: https://github.com/nirum-lang/nirum-python/issues/79
[nirum-python #88]: https://github.com/nirum-lang/nirum-python/pull/88
[nirum-python #92]: https://github.com/nirum-lang/nirum-python/pull/92
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

[#40]: https://github.com/nirum-lang/nirum/issues/40
[#53]: https://github.com/nirum-lang/nirum/pull/53
[#57]: https://github.com/nirum-lang/nirum/pull/57
[#64]: https://github.com/nirum-lang/nirum/pull/64
[#65]: https://github.com/nirum-lang/nirum/issues/65
[#69]: https://github.com/nirum-lang/nirum/issues/69
[#71]: https://github.com/nirum-lang/nirum/pull/71
[#73]: https://github.com/nirum-lang/nirum/pull/73
[#81]: https://github.com/nirum-lang/nirum/pull/81

### Python target

 -  Services became to have their own client implementation of a name with
    `_Client` postfix when they are compiled to Python. [[#52]]
 -  Generated types became to have `__hash__()` method so that they are now
    hashable.  [[#75], [#76]]
 -  Fixed a bug that a Python class generated from a parameterless tag had
    been broken.  [[#55], [#66]]

[#52]: https://github.com/nirum-lang/nirum/pull/52
[#75]: https://github.com/nirum-lang/nirum/issues/75
[#76]: https://github.com/nirum-lang/nirum/pull/76
[#66]: https://github.com/nirum-lang/nirum/pull/66
[#55]: https://github.com/nirum-lang/nirum/issues/55

### Et cetera

 -  Introduced the official Docker image.  The image repository is located to
    [spoqa/nirum](https://hub.docker.com/r/spoqa/nirum/).
    [[#48] by Minyoung Jeong]

[#48]: https://github.com/nirum-lang/nirum/pull/48



Version 0.1.0
-------------

Initial and unstable release for a
[demo session at PyCon APAC 2016][pycon-apac-2016].
Released on August 14, 2016.

[pycon-apac-2016]: https://www.pycon.kr/2016apac/program/36
