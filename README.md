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
defaultWriters "code" = let writeCodeString      = intercalate "\n" . concatMap writeCodeBlock
                            blocks (Pandoc m bs) = bs
                        in  dropWhile (== '\n') . writeCodeString . blocks
defaultWriters writer = case lookup writer writers of
                            Just (PureStringWriter w) -> w (def {writerColumns = 80})
                            _ -> error $ "Pandoc writer '" ++ writer ++ "' not found."
```
