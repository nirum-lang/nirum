Docs target
===========

This target does not generate any program code files, but HTML pages (and
some extra assets like CSS).  It generates three kinds of pages:

 -  A home page that renders *README.md* file (if exists) or table of contents
    (if there is no *README.md* file).
 -  Reference docs from the docs comments in the source code: type definitions,
    union tags, enum members, service methods, etc, and
 -  Manual pages from CommonMark (i.e., _\*.md_) files.


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


Manual pages
------------

The docs target scans all CommonMark (i.e., _\*.md_) files from the same level
to *package.toml* manifest file and its subdirectories, and transforms them
to HTML pages.


Settings
--------

### `title` (required): Docs title

It goes to `<title>` elements of generated HTML pages.  It's usually a name of
the Nirum package.

### `opengraphs` (optional): OpenGraph

It goes to `<meta>` elements of generated HTML pages. It generate OpenGraph
objects.


### `style` (optional): Custom CSS

It goes to very ending of the CSS file, which means it can override other
predefined style rules.


### `header`/`footer` (optional): Custom header & footer HTML

It goes to the very beginning and ending of `<body>` contents.  It's usually
used to customize HTML pages.
