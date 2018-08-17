Annotations
===========

Annotations can be attached to any declarations, and purpose to be used
as directive or metadata by target compiler or runtime library.

Each annotation starts with an `@` character, and its name follows,
e.g., `@foo`.  Annotation names have the same rule to other Nirum identifiers,
but there's no separation of facial and behind names.

There are annotations defined and used by a target compiler, or more than one
targets.  If an annotation that no target defined is used, they are merely no-op
(i.e., ignored and even not errored).

Nirum annotations are similar to Java's one or C#'s attributes.


Declarations
------------

All declarations in Nirum can attach annotations.  Declarations in Nirum mean
things that have their own name, e.g., imports, records, fields, union tags,
service methods, method parameters.  In other words, any things that can have
docs comments also can have annotations.

Each declaration is allowed to have multiple annotations, but their name cannot
be duplicated.

For example, the following code is allowed:

~~~~~~~~ nirum
@foo
@bar(baz = "qux")
unboxed filename (text);
~~~~~~~~

Whereas the following code is disallowed:

~~~~~~~~ nirum
@foo
@foo(baz = "qux")  // `foo` is duplicated
unboxed filename (text);
~~~~~~~~


Arguments
---------

Annotations have zero or more arguments.  Every parameter is named, and names
cannot be duplicated in an annotation.  Annotation arguments follow
an annotation name and are wrapped by parentheses.  An `=` operator is placed
between each parameter name and its argument value.  Arguments are separated
by a comma, and an optional trailing comma is also allowed.  E.g.:

~~~~~~~~ nirum
@foo()
@bar(baz = "qux")
@quux(corge = "grault", garply = "waldo", fred = "plugh",)
unboxed filename (text);
~~~~~~~~

Currently, argument values are only allowed to be a text (which is wrapped by
double quotes), but they could be extended to other types (e.g., integer).
Note that C-style escape sequences work between double quotes, e.g.:

~~~~~~~~ nirum
@foo(bar = "Escape double quotes (\") with backslashes (\\).")
unboxed filename (text);
~~~~~~~~


Common annotations
------------------

The following annotations are common among more than one target languages.
For target-specific annotations, read each target's docs.


### `@docs` (`docs`)                                                     {#docs}

`@docs` annotations represent docs comments (`# ...`).  The following two
examples are equivalent, and the former is internally transformed to the latter:

~~~~~~~~ nirum
# Docs comments are transformed to `@docs` annotations.
unboxed filename (text);
~~~~~~~~

~~~~~~~~ nirum
@docs(docs = "Docs comments are transformed to `@docs` annotations.\n")
unboxed filename (text);
~~~~~~~~

It is primarily for simplifying internal syntax tree, but can be used at runtime
for other purposes.

Since all docs comments become transformed to `@docs` annotations,
a declaration cannot have both a docs comment and a `@docs` annotation at
a time.

`docs`
:   A docs text.


### `@error`                                                            {#error}

Many object-oriented languages (e.g., Java, Python) require exception classes
to inherit a specific base class (e.g., `Exception`) to be thrown.
`@error` annotation is a hint for such languages to make a record or a union to
inherit that.

For example, the following first Nirum code is compiled to the second Python
code:

~~~~~~~~ nirum
@error
union file-error = file-not-found
                 | file-not-readable
                 ;
~~~~~~~~

~~~~~~~~ python
class FileError(Exception):
    ...

class FileNotFound(FileError):
    ...

class FileNotReadable(FileError):
    ...
~~~~~~~~

### `@numeric-constraints`                                {#numeric-constraints}

`@numeric-constraints` annotation constrain the range of the input value.
Currently, available annotation arguments are below:

`min`: Minimum input value; inclusive

`max`: Maximum input value; inclusive

For example, the following first Nirum code is compiled to the second Python
code:

~~~~~~~~ nirum
@numeric-constraints(min=1, max=12)
unboxed month (int32);
~~~~~~~~

~~~~~~~~ python
class Month(object):
    ...
    def __init__(self, value: '__builtin__.int') -> None:
        ...
        if not (value <= (12)):
            raise ValueError("value is greater than 12")
        if not (value >= (1)):
            raise ValueError("value is less than 1")
        ...
 ~~~~~~~~
