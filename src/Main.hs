#!/usr/bin/env runhaskell

import System.Environment (getArgs)

import Data.List (intercalate)

import Text.Pandoc ( Pandoc(Pandoc), Block(CodeBlock, Header)
                   , nullMeta
                   , readMarkdown
                   , writeMarkdown
                   )
import Text.Pandoc.Options ( def
                           , readerApplyMacros
                           , writerColumns
                           )
import Text.Pandoc.Error (handleError)
import Text.Pandoc.Tangle

--- Main Functionality
--- ------------------

main :: IO ()
main = do (tangleArgs, writeArgs, fileNames) <- getArgs >>= return . splitArgInput
          case (defaultTanglers tangleArgs, defaultWriters writeArgs, fileNames) of
            (Nothing, _, _)      -> error $    "Tangler '"
                                            ++ intercalate " " tangleArgs
                                            ++ "' not found in 'defaultTanglers'."
            (Just _, Nothing, _) -> error $    "Writer '"
                                            ++ intercalate " " writeArgs
                                            ++ "' not found in 'defaultWriters'."
            (Just tangler, Just writer, [fileName])
                                 ->     readFile fileName
                                    >>= return . tangler . pandocReader
                                    >>= putStrLn . dropWhile (== '\n') . writer
            (Just _, Just _, _)  -> error $ "Supply exacly one filename."

splitArgInput :: [String] -> ([String], [String], [String])
splitArgInput input = let initArgs  = takeWhile (/= "--")
                          otherArgs = dropWhile (== "--") . dropWhile (/= "--")
                      in  ( initArgs input
                          , initArgs . otherArgs $ input
                          , initArgs . otherArgs . otherArgs $ input
                          )

--- Default Tanglers
--- ----------------

defaultTanglers :: [String] -> Maybe (Pandoc -> Pandoc)
defaultTanglers ["id"]           = Just $ id
defaultTanglers ["code"]         = Just $ dropSectWithoutCode
defaultTanglers ("code" : codes) = Just $ dropSectWithoutCode . takeCodes codes
defaultTanglers _                = Nothing

--- Default Readers
--- ---------------

pandocReader :: String -> Pandoc
pandocReader = handleError . readMarkdown (def {readerApplyMacros = True})

--- Default Writers
-------------------

defaultWriters :: [String] -> Maybe (Pandoc -> String)
defaultWriters ["text"] = Just $ textWriter
defaultWriters ["code"] = Just $ codeWriter
defaultWriters _        = Nothing

textWriter :: Pandoc -> String
textWriter = writeMarkdown (def {writerColumns = 80})

codeWriter :: Pandoc -> String
codeWriter = intercalate "\n" . concatMap writeCodeBlock . getBlocks . dropSectWithoutCode

writeCodeBlock :: Block -> [String]
writeCodeBlock (CodeBlock _ code) = "" : lines code
writeCodeBlock h@(Header n _ _)   = let comment = map ("--- " ++) . lines . textWriter
                                        docStrs = comment (Pandoc nullMeta [h])
                                    in  if n == 1
                                            then "" : "" : docStrs
                                            else "" : docStrs
writeCodeBlock _                  = []
