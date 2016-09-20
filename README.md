# hpqtypes [![Hackage version](https://img.shields.io/hackage/v/hpqtypes.svg?label=Hackage)](https://hackage.haskell.org/package/hpqtypes) [![Build Status](https://secure.travis-ci.org/scrive/hpqtypes.svg?branch=master)](http://travis-ci.org/scrive/hpqtypes)

Efficient and easy-to-use bindings to (slightly modified) libpqtypes,
lipq extension that adds support for binary transport format and
composite types.

Since modified libpqtypes is used, its source code is bundled along
with the bindings. The differences between verbatim libpqtypes and the
one used by this package:

* per-thread global error structures were replaced by explicit passing
  of these structures around so that there is no need to use bound
  threads.

* handlers that take values to be put into the database were modified
  to always expect pointers to objects, as opposed to previous
  situation where primitives were being taken by value (which was
  convenient if the library was used directly from C, but created
  inconsistency problems while trying to define bindings in a sensible
  way).

Examples can be found in the [examples](https://github.com/scrive/hpqtypes/tree/master/examples) directory.
