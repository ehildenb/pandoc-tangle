--- Pandoc Tangle
--- =============

--- Main Functionality
--- ------------------


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
                           , writerColumns
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


defaultReaders :: String -> String -> IO Pandoc
defaultReaders reader = case lookup reader readers of
                            Just (StringReader r) -> fmap (fmap handleError) (r (def {readerApplyMacros = True}))
                            _                     -> error $ "Pandoc reader '" ++ reader ++ "' not found."


--- Tanglers
--- --------

--- ### Sections


takeSect :: String -> Pandoc -> Pandoc
takeSect = takeSects . map (B.toList . B.text) . splitOn "|"


--- ### Code


takeCode :: String -> Pandoc -> Pandoc
takeCode = takeCodes . splitOn "|"


--- Default Writers
--- ---------------


defaultWriters :: String -> Pandoc -> String
defaultWriters ('c' : 'o' : 'd' : 'e' : '-' : lang)
    = let writeCodeString = intercalate "\n" . concatMap (writeCodeBlock lang)
          blocks (Pandoc m bs) = bs
      in  dropWhile (== '\n') . writeCodeString . blocks . dropSectWithoutCode
defaultWriters writer
    = case lookup writer writers of
        Just (PureStringWriter w) -> w (def {writerColumns = 80})
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
        commentL "maude"   = "--- "
        commentL "k"       = "// "
        commentL "c"       = "// "
        commentL "c++"     = "// "
        commentL "bash"    = "# "
        commentL "sh"      = "# "
        commentL "zsh"     = "# "
        commentL "python"  = "# "
        commentL l         = error $ "Commenting for language '" ++ l ++ "' not supported."

