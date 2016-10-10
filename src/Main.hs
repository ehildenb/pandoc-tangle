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
                                  , many , some , (<>) , str , optional
                                  )

import Text.Pandoc         ( Pandoc(Pandoc), Block(CodeBlock, Header)
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
       in  do Tangler reader writer sect code files <- execParser opts
              let codeStrip = maybe id takeCode code
              let sectStrip = maybe id takeSect sect
              input <- case files of
                        [] -> getContents
                        _  -> mapM readFile files >>= return . intercalate "\n"
              defaultReaders reader input >>= putStrLn . defaultWriters writer . codeStrip . sectStrip

data Tangler = Tangler { reader :: String
                       , writer :: String
                       , sect   :: Maybe String
                       , code   :: Maybe String
                       , files  :: [String]
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
    | lang `elem` ls = "" : lines code
writeCodeBlock lang h@(Header n _ _)
    = let writeMD = defaultWriters "markdown"
          comment = map (commentL lang ++) . lines . writeMD . Pandoc nullMeta $ [h]
        in  if n == 1
                then "" : "" : comment
                else "" : comment
    where
        commentL "haskell" = "--- "
        commentL "maude"   = "--- "
        commentL "k"       = "// "
        commentL "c"       = "// "
        commentL "c++"     = "// "
        commentL "bash"    = "# "
        commentL "python"  = "# "
        commentL l         = error $ "Commenting for language '" ++ l ++ "' not supported."
writeCodeBlock _ _ = []
