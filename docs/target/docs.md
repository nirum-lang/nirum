Docs target
===========

This target does not generate any program code files, but HTML pages (and
some extra assets like CSS).  It generates a kind of API reference docs for
the given Nirum package: type definitions, union tags, enum members,
service methods, and so on.


Docs comments
-------------

Nirum provide two kinds of comments.  One is an ordinary comment which starts
with `//`, and other one is a docs comment which begins with `#`.  Docs target
is only aware of the latter.

Docs comments are, unlike ordinal comments, not allowed to any places, but only
allowed to some specific places.

Docs comments are actually a syntactic sugar of [`@docs`](../annotation.md#docs)
annotation, hence allowed to be attached to only where annotations are allowed:
declarations that have a name, e.g., types, fields, members, services, methods,
parameters, modules.

You can find *examples/shapes.nrm* to see examples of docs comments.


Settings
--------

### `title` (required): Docs title

It goes to `<title>` elements of generated HTML pages.  It's usually a name of
the Nirum package.
