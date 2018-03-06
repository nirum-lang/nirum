Python target
=============

Python is Nirum's most actively maintained target language.  If you want to
evaluate Nirum's every feature Python would be the best choice.


Settings
--------

Nirum's Python target provides its own settings.  The below settings can be
configured in package.toml's `targets.python.*` fields.


### `name` (required): PyPI distribution name

Whereas Nirum packages don't have a package name, generated Python packages
need its own unique name.  They are called by several terms: package name,
distribution name, PyPI handle, etc.  It's used to refer the package to
install in `pip install` command, and placed following
<https://pypi.python.org/pypi/> URL.

~~~~~~~~ toml
# Example
[targets.python]
name = "py-foobar"  # will be submitted to: pypi.python.org/pypi/py-foobar
~~~~~~~~


### `minimum_runtime`: Ensure Python runtime library's minimum version

Generated Python object code depends on Python [nirum][] package, the runtime
library for Nirum.  As it's separately distributed, you might face with subtle
incompatibilities between versions.  In order to prevent such incompatibilities
by ensuring the minimum version, Python target provides a `minimum_runtime`
option.  It takes a version string of [nirum][] package which follows [Semantic
Versioning][semver].

~~~~~~~~ toml
# Example
[targets.python]
name = "py-foobar"
minimum_runtime = "0.3.9"  # requires nirum >= 0.3.9
~~~~~~~~

The configured version specifier goes to `install_requires` list of setup.py
script.

[nirum]: https://pypi.python.org/pypi/nirum
[semver]: http://semver.org/


### `renames`: Rename module paths

Sometimes you may need to use other name in Python than package names defined
by Nirum IDL.  For example, you may choose a general term `statistics` for
a module name, but need to use an other name in Python since it's reserved
by Python standard library.  In case, `renames` configuration replace package
names when it's compiled to Python.

It's a table of strings where keys are module paths to be replaced and
values are module paths to replace with.  Note that keys and values are
not Python import paths but Nirum IDL module paths.  That means you can't use
names like `__foo__` because Nirum IDL doesn't allow more than twice continued
underscores/hyphens.

The following example replaces `statistics` to `rpc.statistics`:

~~~~~~~~ toml
[targets.python.renames]
statistics = "rpc.statistics"
~~~~~~~~

The `renames` table is recursively applied to submodules.  If you have 4 modules
and submodules like `statistics`, `statistics.products`, `statistics.users`, and
`statistics.users.friends`, they are renamed to `rpc.statistics.products`,
`rpc.statistics`, `rpc.statistics.products`, `rpc.statistics.users`, and
`rpc.statistics.users.friends`.

Though it's applied only from root modules to submodules.  Even if there're
some matched module paths in the middle they aren't renamed.  For example,
whereas `statistics.foo` is renamed to `rpc.statistics.foo`, `foo.statistics`
is remained without renaming.

Names to be replaced can contain periods.  For example, the following example
renames `foo.bar.baz` to `new-name.baz`:

~~~~~~~~ toml
[targets.python.renames]
"foo.bar" = "new-name"  # Note that the key is quoted.
~~~~~~~~


Entry points
------------

All generated Python modules and classes are registered as setuptools'
[entry points] so that an application can discover them at runtime.
There are two groups:

`nirum.modules`
:   It maps Nirum modules to Python modules.  Nirum module paths are normalized
    to avoid underscores and upper letters and use hyphens and lower letters
    instead, e.g., `foo-bar.baz`.   The table works well with `renames`
    settings as well.

`nirum.classes`
:   It maps Nirum types (including services) to Python classes.  Nirum type
    names are qualified and their leading module paths are also normalized
    (the same rule to `nirum.modules` applies here).

Here's an example to discover Python modules generated from Nirum modules.

~~~~~~~~ python
>>> import pkg_resources  # provided by setuptools
>>> module_entry_points = list(pkg_resources.iter_entry_points('nirum.modules'))
>>> module_entry_points
[EntryPoint.parse('foo = foo'),
 EntryPoint.parse('foo.bar = foo.bar'),
 EntryPoint.parse('qux = qux'),
 ...]
>>> module_entry_points[1]
EntryPoint.parse('foo.bar = foo.bar')
>>> module_entry_points[0].resolve()
<module 'foo.bar' from '~/foo/bar/__init__.py'>
~~~~~~~~

Note that you need to call `.resolve()` method as well.

Finding a Python class generated from a Nirum type can be in a similar way:

~~~~~~~~ python
>>> list(pkg_resources.iter_entry_points('nirum.classes', 'foo.NoSuchName'))
[]
>>> list(pkg_resources.iter_entry_points('nirum.classes', 'foo.bar.some-type'))
[EntryPoint.parse('foo.bar.some-type = foo.bar:SomeType')]
>>> _[0].resolve()
<class 'foo.bar.SomeType'>
~~~~~~~~

As you can see, the second parameter of `pkg_resources.iter_entry_points()`
purposes to filter entry points by the given name.

[entry points]: https://setuptools.readthedocs.io/en/latest/pkg_resources.html#entry-points
