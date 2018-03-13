Elm target
==========

[Elm] is a purely functional programming language for single page webapps.
Nirum currently has an experimental implementation for targetting Elm.

[Elm]: http://elm-lang.org/


Settings
--------

The below settings can be configured in *package.toml*'s `targets.elm.*` fields.


### `repository` (required): Git repository to serve the compiled package

It's used as `"repository"` field of a generated *package.json* manifest.
It should refer to the repository of a *generated* Elm package,
*not the Nirum source package*.
