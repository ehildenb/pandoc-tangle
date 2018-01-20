--- Pandoc Tangle
--- =============

--- This library allows for tangling documents in Pandoc's internal representation.
--- A set of default tanglers can be accessed with the command `pandoc-tangle`.

--- See [the Tangle library](TangleLib.md) for an overview of the library
--- functionality (for developing).

--- Main Functionality
--- ------------------

--- The default tangler imports `Pandoc` and the library tangler to implement some
--- simple defaults.

--- Run `stack setup`, followed by `stack build` to build the executable. You can
--- run `stack install` to place it at `~/.local/bin`.

--- See `pandoc-tangle --help` for usage.


module Main where

import Data.List ( intercalate )
import Data.List.Split ( splitOn )
import Data.Monoid ((<>))

import System.IO ( getContents )

import Options.Applicative as OPT ( execParser , Parser , ParserInfo
                                  , strOption , subparser , command , argument
                                  , long , short , metavar , help
                                  , helper , info , fullDesc , progDesc , header
                                  , many , some , str , optional , switch
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


--- Default Readers
--- ---------------

--- Only the default `readers` [from
--- Pandoc](https://hackage.haskell.org/package/pandoc-1.17.2/docs/Text-Pandoc.html#g:4)
--- are supported. The `READER` specified on the command-line will be looked up in
--- the list of Pandoc readers.


defaultReaders :: String -> String -> IO Pandoc
defaultReaders reader = case lookup reader readers of
                            Just (StringReader r) -> fmap (fmap handleError) (r (def {readerApplyMacros = True}))
                            _                     -> error $ "Pandoc reader '" ++ reader ++ "' not found."


--- Tanglers
--- --------

--- ### Sections

--- The `SECTIONS` input is the name of a section to keep in the document.
--- Super-sections will be preserved as well. To include multiple sections separate
--- them with `|`, eg "Section 1|Section 4.3" would grab both "Section 1" and
--- "Section 4.3".


takeSect :: String -> Pandoc -> Pandoc
takeSect = takeSects . map (B.toList . B.text) . splitOn "|"


--- ### Code

--- The `CODEBLOCKS` input is the class of code-blocks to keep in the document. Any
--- code-blocks not labelled with `CODEBLOCKS` will not be kept. To include multiple
--- code classes separate them with `|`, eg "main|lib" would include code classes
--- marked with either "main" or "lib".


takeCode :: String -> Pandoc -> Pandoc
takeCode = takeCodes . splitOn "|"


--- Default Writers
--- ---------------

--- The default `writers` [from
--- Pandoc](https://hackage.haskell.org/package/pandoc-1.17.2/docs/Text-Pandoc.html#g:4)
--- are supported. The `WRITER` specified on the command-line will be looked up in
--- the list of Pandoc writers.

--- In addition, the `code-LANG` writer will produce just the code, where `LANG` is
--- the programming language comment-style to use for headers.


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
        commentL "go"      = "// "
        commentL "bash"    = "# "
        commentL "sh"      = "# "
        commentL "zsh"     = "# "
        commentL "python"  = "# "
        commentL l         = error $ "Commenting for language '" ++ l ++ "' not supported."

