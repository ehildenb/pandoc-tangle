--- Default Tangler
--- ===============

module Main where

import Data.List ( intercalate )

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
import Text.Pandoc.Tangle

--- Main Functionality
--- ------------------

main :: IO ()
main = let opts = info (helper <*> tanglerOpts)
                       (  fullDesc
                       <> progDesc "Use Pandoc, as a Tangler."
                       <> OPT.header "pandoc-tangle - a tangler for Pandoc."
                       )
       in  do Tangler reader code writer files <- execParser opts
              input <- case files of
                        [] -> getContents
                        _  -> mapM readFile files >>= return . intercalate "\n"
              defaultReaders reader input >>= putStrLn . defaultWriters writer . keepCode code

data Tangler = Tangler { reader  :: String
                       , code    :: Maybe String
                       , writer  :: String
                       , files   :: [String]
                       }

tanglerOpts :: Parser Tangler
tanglerOpts = Tangler <$> strOption (  long "from"
                                    <> short 'f'
                                    <> metavar "READER"
                                    <> help "Pandoc READER to use."
                                    )
                      <*> ( optional $ strOption (  long "code"
                                                 <> short 'c'
                                                 <> metavar "CODEBLOCKS"
                                                 <> help "CODEBLOCKS to keep."
                                                 )
                          )
                      <*> strOption (  long "to"
                                    <> short 't'
                                    <> metavar "WRITER"
                                    <> help "Pandoc WRITER to use."
                                    )
                      <*> many (argument OPT.str (metavar "FILES..."))

--- Default Readers
--- ---------------

defaultReaders :: String -> String -> IO Pandoc
defaultReaders reader = case lookup reader readers of
                            Just (StringReader r) -> fmap (fmap handleError) (r (def {readerApplyMacros = True}))
                            _                     -> error $ "Pandoc reader '" ++ reader ++ "' not found."

--- Code Stripper
--- -------------

keepCode :: Maybe String -> Pandoc -> Pandoc
keepCode Nothing     = id
keepCode (Just code) = dropSectWithoutCode . takeCodes [code]

--- Default Writers
--- ---------------

defaultWriters :: String -> Pandoc -> String
defaultWriters "code" = let writeCodeString      = intercalate "\n" . concatMap writeCodeBlock
                            blocks (Pandoc m bs) = bs
                        in  dropWhile (== '\n') . writeCodeString . blocks
defaultWriters writer = case lookup writer writers of
                            Just (PureStringWriter w) -> w (def {writerColumns = 80})
                            _ -> error $ "Pandoc writer '" ++ writer ++ "' not found."

writeCodeBlock :: Block -> [String]
writeCodeBlock (CodeBlock (_,ls,_) code)
    = "" : lines code
writeCodeBlock h@(Header n _ _)
    = let writeMD = defaultWriters "markdown"
          comment = map (commentL "haskell" ++) . lines . writeMD . Pandoc nullMeta $ [h]
        in  if n == 1
                then "" : "" : comment
                else "" : comment
    where
        commentL "haskell" = "--- "
        commentL "maude"   = "--- "
        commentL "c"       = "// "
        commentL "c++"     = "// "
        commentL "bash"    = "# "
        commentL "python"  = "# "
        commentL l         = error $ "Commenting for language '" ++ l ++ "' not supported."
writeCodeBlock _ = []
