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
