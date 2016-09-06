Pandoc Tangle
=============

This library allows for tangling/weaving documents in Pandoc's internal
representation. A set of default tanglers can be accessed with the command
`pandoc-tangle`.

Default Tangler
===============

The default tangler imports `Pandoc` and the library tangler to implement some
simple defaults.

Your input must be of the form:

``` {.sh .example}
$ pandoc-tangle reader tangler writer file_name
```

Main Functionality
------------------

The reader, tangler, and writer will be run and the output printed to stdout.

Default Readers
---------------

Only the default `readers` [from
Pandoc](https://hackage.haskell.org/package/pandoc-1.17.2/docs/Text-Pandoc.html#g:4)
are supported. The `String` fed to `defaultReaders` will be the `READER`
supplied to `pandoc-tangle`.

``` {.haskell .main .example}
defaultReaders :: String -> String -> IO Pandoc
defaultReaders reader = case lookup reader readers of
                            Just (StringReader r) -> fmap (fmap handleError) (r (def {readerApplyMacros = True}))
                            _                     -> error $ "Pandoc reader '" ++ reader ++ "' not found."
```

Code Stripper
-------------

The `Maybe String` fed into this is the `CODEBLOCKS` option. If there is no
supplied `CODEBLOCKS` option, all of the code is kept.

``` {.haskell .main .example}
keepCode :: Maybe String -> Pandoc -> Pandoc
keepCode Nothing     = id
keepCode (Just code) = dropSectWithoutCode . takeCodes [code]
```

Default Writers
---------------

The default writers are listed here.

The `pandoc writer` writer will lookup the supplied `writer` in [Pandoc's
writers](https://hackage.haskell.org/package/pandoc-1.17.2/docs/Text-Pandoc.html#g:4)
and use that.

The `code` writer will produce just the code.

The `String` fed into the `defaultWriters` function will be the `WRITER`
supplied to `pandoc-tangle`.

``` {.haskell .main .example}
defaultWriters :: String -> Pandoc -> String
defaultWriters ('c' : 'o' : 'd' : 'e' : '-' : lang)
    = let writeCodeString = intercalate "\n" . concatMap (writeCodeBlock lang)
          blocks (Pandoc m bs) = bs
      in  dropWhile (== '\n') . writeCodeString . blocks . dropSectWithoutCode
defaultWriters writer
    = case lookup writer writers of
        Just (PureStringWriter w) -> w (def {writerColumns = 80})
        _ -> error $ "Pandoc writer '" ++ writer ++ "' not found."
```

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
