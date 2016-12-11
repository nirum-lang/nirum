Package
=======

Unlike many other RPC framworks/IDL compilers, Nirum's single unit of
compilation is not a source file, but a package of source files.
Each package is compiled to a package of target language (e.g. Python package
with setup.py file).  The following example is an ordinary Nirum package:

 -  /
     -  foo/
         -  bar.nrm
         -  baz.nrm
         -  baz/
             -  qux.nrm
             -  quux.nrm
     -  package.toml

If the target is Python the above package is compiled to the below Python
package:

 -  /
     -  foo/
         -  \_\_init\_\_.py
         -  bar/
             -  \_\_init\_\_.py
         -  baz/
             -  \_\_init\_\_.py
         -  baz/
             -  \_\_init\_\_.py
             -  qux/
                 -  \_\_init\_\_.py
             -  quux/
                 -  \_\_init\_\_.py
     -  setup.py


Package manifest
----------------

Every Nirum package has its own package.toml file.  It contains metadata
about the package (e.g. `version`, `authors`).  The following TOML code
is an example:

    version = "1.0.0"  # (required)

    [[authors]]
    name = "John Doe"  # (required)
    email = "johndoe@example.com/"
    uri = "http://example.com/~johndoe/"

It consists of the following fields (*emphasized fields* are required):

*`version`* (string)
:   The required [semver][] of the package.

`authors` (array of tables)
:   The list of authors.  Note that it can be empty, but `name` field is
    required if any author is provided.  Each author table consists of
    the following fields:

    *`name`* (string)
    :   The required full name of the author.

    `email` (string)
    :   An optional email address of the author.

    `uri` (string)
    :   An optional URI to author's website.

[semver]: http://semver.org/
