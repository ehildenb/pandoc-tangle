Default Tangler
===============

The default tangler imports `Pandoc` and the library tangler to implement some
simple defaults.

Your input must be of the form:

``` {.sh .example}
$ pandoc-tangle reader_arguments -- tangler_arguments -- writer_arguments -- file_name
```

Main Functionality
------------------

The reader, tangler, and writer will be run and the output printed to stdout.

Default Readers
---------------

Only the default `readers` [from
Pandoc](https://hackage.haskell.org/package/pandoc-1.17.2/docs/Text-Pandoc.html#g:4)
are supported. The `[String]` fed to `defaultReaders` will be the
`reader_arguments` supplied to `pandoc-tangle`.

``` {.haskell .main .example}
defaultReaders :: [String] -> String -> IO Pandoc
defaultReaders [reader] = case lookup reader readers of
                            Just (StringReader r) -> fmap (fmap handleError) (r (def {readerApplyMacros = True}))
                            _                     -> error $ "Pandoc reader '" ++ reader ++ "' not found."
defaultReaders _        = error $ "Only default Pandoc readers supported."
```

Default Tanglers
----------------

The default tanglers are listed here.

The simplest of them is the `id` tangler which leaves the document alone.

The `code` tangler removes sections that don't have code in them.

The `code codes*` tangler removes all code blocks not marked with one of the
strings in `codes*`, then removes sections that don't have code.

The `[String]` fed to the `defaultTanglers` function will be the
`tangler_arguments` supplied to `pandoc-tangle`.

``` {.haskell .main .example}
defaultTanglers :: [String] -> Pandoc -> Pandoc
defaultTanglers ["id"]           = id
defaultTanglers ["code"]         = dropSectWithoutCode
defaultTanglers ("code" : codes) = dropSectWithoutCode . takeCodes codes
defaultTanglers tangler          = error $ "Tangler '" ++ intercalate " " tangler ++ "' not found."
```

Default Writers
---------------

The default writers are listed here.

The `pandoc writer` writer will lookup the supplied `writer` in [Pandoc's
writers](https://hackage.haskell.org/package/pandoc-1.17.2/docs/Text-Pandoc.html#g:4)
and use that.

The `code` writer will produce just the code marked with the supplide `lang`
attribute.

The `[String]` fed into the `defaultWriters` function will be the
`writer_arguments` supplied to `pandoc-tangle`.

``` {.haskell .main .example}
defaultWriters :: [String] -> Pandoc -> String
defaultWriters ["pandoc", writer] = case lookup writer writers of
                                        Just (PureStringWriter w) -> w (def {writerColumns = 80})
                                        _ -> error $ "Pandoc writer '" ++ writer ++ "' not found."
defaultWriters ["code", lang]     = let writeCodeString      = intercalate "\n" . concatMap (writeCodeBlock lang)
                                        blocks (Pandoc m bs) = bs
                                        getCode              = dropSectWithoutCode . takeCode lang
                                    in  writeCodeString . blocks . getCode
defaultWriters w                  = error $ "Writer '" ++ intercalate " " w ++ "' not found."
```
