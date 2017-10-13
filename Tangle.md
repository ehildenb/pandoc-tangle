Pandoc Tangle
=============

This library allows for tangling documents in Pandoc's internal representation.
A set of default tanglers can be accessed with the command `pandoc-tangle`.

See [the Tangle library](TangleLib.md) for an overview of the library
functionality (for developing).

### Outstanding Bug

-   Using option `--strip-text` changes what parts of the document make it
    through incorrectly.

Installing
----------

First make sure to have the tool
[Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) installed.
It comes in some package on most Linux distros, and is available via Homebrew on
MacOS. Then clone this repository, run `stack init`, followed by `stack build`,
followed by `stack install`. It will copy an executable to `~/.local/bin` (or
somewhere like that), which you can either copy to somewhere on your `PATH`, or
symlink to from somewhere in your `PATH`.

Example Usage
-------------

These examples can be run in the git repository over the document `Tangle.md`,
from which the rest of the documents in the git repository are derived.

Include sections "Main Functionality" and "Predicates over Documents", but only
keep code marked with "lib" in the resulting document:

``` {.sh .example}
$ pandoc-tangle --from markdown --to markdown --code lib \
                --section 'Main Functionality|Predicates over Documents' Tangle.md
```

Same, but write to HTML instead of Markdown and to include code marked with both
"lib" and "main":

``` {.sh .example}
$ pandoc-tangle --from markdown --to html --code "lib|main" \
                --section 'Main Functionality|Predicates over Documents' Tangle.md
```

Main Functionality
------------------

The default tangler imports `Pandoc` and the library tangler to implement some
simple defaults.

Run `stack init`, followed by `stack build` to build the executable. You can run
`stack install` to place it at `~/.local/bin`.

See `pandoc-tangle --help` for usage.

``` {.haskell .main}
module Main where

import Data.List ( intercalate )
import Data.List.Split ( splitOn )

import System.IO ( getContents )

import Options.Applicative as OPT ( execParser , Parser , ParserInfo
                                  , strOption , subparser , command , argument
                                  , long , short , metavar , help
                                  , helper , info , fullDesc , progDesc , header
                                  , many , some , (<>) , str , optional , switch
                                  )

import Text.Pandoc         ( Pandoc(Pandoc), Block(CodeBlock, Header, Null, Div)
                           , nullMeta
                           , readers, Reader(StringReader)
                           , writers, Writer(PureStringWriter)
                           )
import Text.Pandoc.Options ( def
                           , readerApplyMacros
                           , WrapOption(WrapPreserve)
                           , writerWrapText
                           )
import Text.Pandoc.Error   ( handleError )
import qualified Text.Pandoc.Builder as B ( toList , text )
import Text.Pandoc.Tangle

main :: IO ()
main = let opts = info (helper <*> tanglerOpts)
                       (  fullDesc
                       <> progDesc "Use Pandoc, as a Tangler."
                       <> OPT.header "pandoc-tangle - a tangler for Pandoc."
                       )
       in  do Tangler reader writer sect code stripText flattenDivs files <- execParser opts
              let flatDivs = id
              let transformer = (\(Pandoc m bs) -> Pandoc m $ filter (not . (==) Null) bs)
                              . if stripText then onlyCode else id
                              . maybe id takeCode code
                              . maybe id takeSect sect
                              . if flattenDivs then flatDivs else id
              mapM readFile files >>= defaultReaders reader . intercalate "\n" >>= putStrLn . defaultWriters writer . transformer

data Tangler = Tangler { reader      :: String
                       , writer      :: String
                       , sect        :: Maybe String
                       , code        :: Maybe String
                       , stripText   :: Bool
                       , flattenDivs :: Bool
                       , files       :: [String]
                       }

tanglerOpts :: Parser Tangler
tanglerOpts = Tangler <$> strOption (  long "from"
                                    <> short 'f'
                                    <> metavar "READER"
                                    <> help "Pandoc READER to use."
                                    )
                      <*> strOption (  long "to"
                                    <> short 't'
                                    <> metavar "WRITER"
                                    <> help "Pandoc WRITER to use."
                                    )
                      <*> ( optional $ strOption (  long "section"
                                                 <> short 's'
                                                 <> metavar "SECTIONS"
                                                 <> help "SECTIONS to keep."
                                                 )
                          )
                      <*> ( optional $ strOption (  long "code"
                                                 <> short 'c'
                                                 <> metavar "CODEBLOCKS"
                                                 <> help "CODEBLOCKS to keep."
                                                 )
                          )
                      <*> ( switch $ (  long "strip-text"
                                     <> help "Strip non-header text."
                                     )
                          )
                      <*> ( switch $ (  long "flatten-divs"
                                     <> help "Flatten `div` tags in document."
                                     )
                          )
                      <*> many (argument OPT.str (metavar "FILES..."))
```

Default Readers
---------------

Only the default `readers`
[from Pandoc](https://hackage.haskell.org/package/pandoc-1.17.2/docs/Text-Pandoc.html#g:4)
are supported. The `READER` specified on the command-line will be looked up in
the list of Pandoc readers.

``` {.haskell .main}
defaultReaders :: String -> String -> IO Pandoc
defaultReaders reader = case lookup reader readers of
                            Just (StringReader r) -> fmap (fmap handleError) (r (def {readerApplyMacros = True}))
                            _                     -> error $ "Pandoc reader '" ++ reader ++ "' not found."
```

Tanglers
--------

### Sections

The `SECTIONS` input is the name of a section to keep in the document.
Super-sections will be preserved as well. To include multiple sections separate
them with `|`, eg "Section 1|Section 4.3" would grab both "Section 1" and
"Section 4.3".

``` {.haskell .main}
takeSect :: String -> Pandoc -> Pandoc
takeSect = takeSects . map (B.toList . B.text) . splitOn "|"
```

### Code

The `CODEBLOCKS` input is the class of code-blocks to keep in the document. Any
code-blocks not labelled with `CODEBLOCKS` will not be kept. To include multiple
code classes separate them with `|`, eg "main|lib" would include code classes
marked with either "main" or "lib".

``` {.haskell .main}
takeCode :: String -> Pandoc -> Pandoc
takeCode = takeCodes . splitOn "|"
```

Default Writers
---------------

The default `writers`
[from Pandoc](https://hackage.haskell.org/package/pandoc-1.17.2/docs/Text-Pandoc.html#g:4)
are supported. The `WRITER` specified on the command-line will be looked up in
the list of Pandoc writers.

In addition, the `code-LANG` writer will produce just the code, where `LANG` is
the programming language comment-style to use for headers.

``` {.haskell .main}
defaultWriters :: String -> Pandoc -> String
defaultWriters ('c' : 'o' : 'd' : 'e' : '-' : lang)
    = let writeCodeString = intercalate "\n" . concatMap (writeCodeBlock lang)
          blocks (Pandoc m bs) = bs
      in  dropWhile (== '\n') . writeCodeString . blocks . dropSectWithoutCode
defaultWriters writer
    = case lookup writer writers of
        Just (PureStringWriter w) -> w (def {writerWrapText = WrapPreserve})
        _ -> error $ "Pandoc writer '" ++ writer ++ "' not found."

writeCodeBlock :: String -> Block -> [String]
writeCodeBlock lang (CodeBlock (_,ls,_) code)
    | lang `elem` ls = "" : "" : lines code ++ [""]
writeCodeBlock lang b
    = let writeMD = defaultWriters "markdown"
          comment = map (commentL lang ++) . lines . writeMD . Pandoc nullMeta $ [b]
      in  "" : comment
    where
        commentL "haskell" = "--- "
        commentL "maude"   = "--- ; "
        commentL "k"       = "// "
        commentL "c"       = "// "
        commentL "c++"     = "// "
        commentL "bash"    = "# "
        commentL "sh"      = "# "
        commentL "zsh"     = "# "
        commentL "python"  = "# "
        commentL l         = error $ "Commenting for language '" ++ l ++ "' not supported."
```


Tangle
======

This module can be used to aid tangling documents in Pandoc's internal
representation. Metadata can be used to inform the derivation process, and
various small document manipulators are supplied.

``` {.haskell .lib}
module Text.Pandoc.Tangle where

import Data.List ((\\))
import qualified Data.Map as M (Map, lookup, toList)

import Text.Pandoc      ( Pandoc(Pandoc)
                        , Block(CodeBlock, Header, Para, Null, Div)
                        , Inline(Str, Space, Code, Math)
                        , Attr , nullAttr
                        , Meta(Meta) , MetaValue(MetaMap, MetaInlines, MetaList, MetaString)
                        )
import Text.Pandoc.Walk ( walk, walkM )
```

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

``` {.haskell .lib}
takeSects :: [[Inline]] -> Pandoc -> Pandoc
takeSects names (Pandoc m bs) = Pandoc m $ takeSectWith (sectNames names) bs
```

`takeSectWithCode` will only take sections which have codeblocks in them. This
is useful for creating a minimal "skeleton" document to write a code-file from.

``` {.haskell .lib}
takeSectWithCode :: Pandoc -> Pandoc
takeSectWithCode (Pandoc m bs) = Pandoc m $ takeSectWith (any codeBlock) bs
```

`takeSectWith` accepts a predicate over a section (so a predicate over the list
of blocks that make up a section) and keeps sections which match that predicate.
Note that the predicate is run *only* over the section header and the
section-text, *not* including the sub-section headers or text of that section.
If any of the sub-sections of a section are kept, the section is kept as well.

``` {.haskell .lib}
takeSectWith :: ([Block] -> Bool) -> [Block] -> [Block]
takeSectWith _ [] = []
takeSectWith p (h@(Header n _ _) : bs)
    = let sect      = takeWhile (not . headerN (<= n)) bs
          sectText  = takeWhile (not . header) sect
          takeRest  = takeSectWith p $ dropWhile (not . headerN (<= n)) bs
          subSects  = takeSectWith p $ dropWhile (not . header) sect
          sectFinal = case (p $ h : sectText, subSects) of
                        (True, _)   -> h : sect
                        (False, []) -> []
                        _           -> h : (sectText ++ subSects)
      in  sectFinal ++ takeRest
takeSectWith p bs = let beforeH = takeWhile (not . header) bs
                        afterH  = takeSectWith p . dropWhile (not . header) $ bs
                    in  if p beforeH then beforeH ++ afterH else afterH
```

`takeCodes` will only keep code marked with the specified classes.

``` {.haskell .lib}
takeCodes :: [String] -> Pandoc -> Pandoc
takeCodes codes = walk (onlyCodeClasses codes)

onlyCodeClass :: String -> Block -> Block
onlyCodeClass code b = if codeBlock `implies` isClass code $ b then b else Null

onlyCodeClasses :: [String] -> Block -> Block
onlyCodeClasses codes b = if codeBlock `implies` hasClass codes $ b then b else Null
```

`onlyCode` will keep level 3 or lower headers, along with codeblocks.

``` {.haskell .lib}
onlyCode :: Pandoc -> Pandoc
onlyCode = walk onlyCode'
    where
        onlyCode' h@(Header n _ _)
            | n <= 3                 = h
        onlyCode' cb@(CodeBlock _ _) = cb
        onlyCode' _                  = Null
```

### Drop Functions

The `drop*` functions will look for sections with certain properties and drop
those sections and all sub-sections.

`dropSects` will remove all sections which have names in the supplied list of
section names. It uses `dropSect`, which removes only one section name from a
document.

``` {.haskell .lib}
dropSects :: [[Inline]] -> Pandoc -> Pandoc
dropSects = foldl (\bf n -> bf . dropSect n) id

dropSect :: [Inline] -> Pandoc -> Pandoc
dropSect name (Pandoc m bs)
    = let beforeSect = takeWhile (not . headerName name) bs
          afterSect  = case dropWhile (not . headerName name) bs of
                           []                      -> []
                           (h@(Header n _ _) : bs) -> dropWhile (not . headerN (<= n)) bs
      in  Pandoc m $ beforeSect ++ afterSect
```

`dropClasses` will remove the listed class attributes from the code-blocks and
headers to avoid clutter in the final document.

``` {.haskell .lib}
dropClasses :: [String] -> Block -> Block
dropClasses classes (CodeBlock (_, cs, _) code) = CodeBlock ("", cs \\ classes, []) code
dropClasses classes (Header n  (_, cs, _) h)    = Header n  ("", cs \\ classes, []) h
dropClasses classes b                           = b
```

`dropSectWithoutCode` will remove any section that contains no code.

``` {.haskell .lib}
dropSectWithoutCode :: Pandoc -> Pandoc
dropSectWithoutCode (Pandoc m bs) = Pandoc m $ takeSectWith (any codeBlock) bs
```

`dropMath` will remove all math from a document. It's useful if you have some
target which doesn't handle math very well.

``` {.haskell .lib}
dropMath :: Pandoc -> Pandoc
dropMath
    = let nullMath (Para [(Math _ _)]) = Null
          nullMath b                   = b
      in  walk nullMath
```

`flatDivs` will flatten all `div` in a document into their parents.

``` {.haskell .lib}
flatDivs :: Pandoc -> Pandoc
flatDivs (Pandoc m bs) = Pandoc m (walkM flatDivs' (Div nullAttr bs))
    where
        flatDivs' (Div _ bs) = bs
        flatDivs' b          = [b]
```

Predicates over Documents
-------------------------

``` {.haskell .lib}
sectNames :: [[Inline]] -> [Block] -> Bool
sectNames names = or . concatMap (\b -> map (flip headerName b) names)

codeBlock :: Block -> Bool
codeBlock (CodeBlock _ _) = True
codeBlock _               = False

header :: Block -> Bool
header (Header _ _ _) = True
header _              = False

headerN :: (Int -> Bool) -> Block -> Bool
headerN p (Header n _ _) = p n
headerN _ b              = False

headerName :: [Inline] -> Block -> Bool
headerName is (Header _ _ name) = is == name
headerName _  _                 = False

--- predicates over attributes
hasClass :: [String] -> Block -> Bool
hasClass strs b = or $ map (flip isClass b) strs

isClass :: String -> Block -> Bool
isClass s b = let (_, cs, _) = getAttr b
              in  s `elem` cs

getAttr :: Block -> Attr
getAttr (Header _ attr _)  = attr
getAttr (CodeBlock attr _) = attr
getAttr _                  = nullAttr

--- implication predicate builder
implies :: (a -> Bool) -> (a -> Bool) -> a -> Bool
implies p1 p2 a = if p1 a then p2 a else True
```

Metadata Extraction
-------------------

``` {.haskell .lib}
--- make a field optional with a default
withDefault :: a -> Maybe a -> Maybe a
withDefault _ (Just a) = Just a
withDefault a Nothing  = Just a

--- extract exact key
key :: String -> MetaValue -> Maybe MetaValue
key k (MetaMap m) = M.lookup k m
key _ _           = Nothing

keyOf :: String -> (MetaValue -> Maybe a) -> MetaValue -> Maybe a
keyOf k t m = key k m >>= t

--- extract associative map
-- (`key: value` pair where we know the structure of `value` but not the `key`)
assocMap :: (MetaValue -> Maybe a) -> MetaValue -> Maybe [(String, a)]
assocMap f (MetaMap m) = traverse (\(k, v) -> fmap ((,) k) $ f v) $ M.toList m
assocMap _ _           = Nothing

--- extract simple list of values
list :: MetaValue -> Maybe [MetaValue]
list (MetaList ms) = Just ms
list _             = Nothing

listOf :: (MetaValue -> Maybe a) -> MetaValue -> Maybe [a]
listOf f m = case f m of
                Just a  -> Just [a]
                Nothing -> list m >>= traverse f

--- inlines
inlines :: MetaValue -> Maybe [Inline]
inlines (MetaString s)   = Just [Str s]
inlines (MetaInlines is) = Just is
inlines _                = Nothing

--- strings
str :: MetaValue -> Maybe String
str = fmap inlinesToStr . inlines

int :: MetaValue -> Maybe Integer
int = fmap read . str

--- convert inlines to strings
inlinesToStr :: [Inline] -> String
inlinesToStr = concatMap inlineToStr

inlineToStr :: Inline -> String
inlineToStr (Str s)       = s
inlineToStr Space         = " "
inlineToStr (Code _ code) = "`" ++ code ++ "`"
inlineToStr _             = ""
```
