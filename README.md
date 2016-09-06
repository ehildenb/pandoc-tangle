Pandoc Tangle
=============

This library allows for tangling documents in Pandoc's internal representation.
A set of default tanglers can be accessed with the command `pandoc-tangle`.

Main Functionality
------------------

The default tangler imports `Pandoc` and the library tangler to implement some
simple defaults.

Run `stack init`, followed by `stack build` to build the executable. You can run
`stack install` to place it at `~/.local/bin`.

See `pandoc-tangle --help` for usage.

Default Readers
---------------

Only the default `readers` [from
Pandoc](https://hackage.haskell.org/package/pandoc-1.17.2/docs/Text-Pandoc.html#g:4)
are supported. The `READER` specified on the command-line will be looked up in
the list of Pandoc readers.

``` {.haskell .main .example}
defaultReaders :: String -> String -> IO Pandoc
defaultReaders reader = case lookup reader readers of
                            Just (StringReader r) -> fmap (fmap handleError) (r (def {readerApplyMacros = True}))
                            _                     -> error $ "Pandoc reader '" ++ reader ++ "' not found."
```

Tanglers
--------

### Sections

The `SECTIONS` input is the name of a section to keep in the document.
Super-sections will be preserved as well.

### Code

The `CODEBLOCKS` input is the class of code-blocks to keep in the document. Any
code-blocks not labelled with `CODEBLOCKS` will not be kept.

Default Writers
---------------

The default `writers` [from
Pandoc](https://hackage.haskell.org/package/pandoc-1.17.2/docs/Text-Pandoc.html#g:4)
are supported. The `WRITER` specified on the command-line will be looked up in
the list of Pandoc writers.

In addition, the `code-LANG` writer will produce just the code, where `LANG` is
the programming language comment-style to use for headers.

Tangle
======

This module can be used to aid tangling documents in Pandoc's internal
representation. Metadata can be used to inform the derivation process, and
various small document manipulators are supplied.

Document Manipulation
---------------------

These functions are of type `Pandoc -> Pandoc`, so they can be used to build up
document manipulations using function composition.

### Take Functions

In general, the `take*` functions will look for sections that have certain
properties and keep those sections along with all super-sections. Super-sections
text which is not part of one of it's sub-sections will be kept as well.

The `takeSects` function will only keep sections of the document which are in
the supplied list of section names. It will also take super-sections of any
section that is kept.

`takeSectWithCode` will only take sections which have codeblocks in them. This
is useful for creating a minimal "skeleton" document to write a code-file from.

`takeSectWith` accepts a predicate over a section (so a predicate over the list
of blocks that make up a section) and keeps sections which match that predicate.
Note that the predicate is run *only* over the section header and the
section-text, *not* including the sub-section headers or text of that section.
If any of the sub-sections of a section are kept, the section is kept as well.

`takeCode` will only keep code marked with the specified class. `takeCode` takes
code marked with any in the list of classes

### Drop Functions

The `drop*` functions will look for sections with certain properties and drop
those sections and all sub-sections.

`dropSects` will remove all sections which have names in the supplied list of
section names. It uses `dropSect`, which removes only one section name from a
document.

`dropClasses` will remove the listed class attributes from the code-blocks and
headers to avoid clutter in the final document.

`dropSectWithoutCode` will remove any section that contains no code.

`dropMath` will remove all math from a document. It's useful if you have some
target which doesn't handle math very well.

Predicates over Documents
-------------------------

Metadata Extraction
-------------------
