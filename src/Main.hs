--- Default Tangler
--- ===============

module Main where

import System.Environment (getArgs)

import Data.List (intercalate)

import Text.Pandoc ( Pandoc(Pandoc), Block(CodeBlock, Header)
                   , nullMeta
                   , readers, Reader(StringReader)
                   , writers, Writer(PureStringWriter)
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
main = do (rArgs : tArgs : wArgs : (fName : []) : _) <- getArgs >>= return . splitArgInput
          let processor = defaultWriters wArgs . defaultTanglers tArgs
          readFile fName >>= defaultReaders rArgs >>= putStrLn . dropWhile (== '\n') . processor

splitArgInput :: [String] -> [[String]]
splitArgInput = let initArgs       = takeWhile (/= "--")
                    otherArgs args = case dropWhile (/= "--") args of
                                        ("--" : rest) -> rest
                                        _             -> []
                in  map initArgs . iterate otherArgs

--- Default Readers
--- ---------------

defaultReaders :: [String] -> String -> IO Pandoc
defaultReaders [reader] = case lookup reader readers of
                            Just (StringReader r) -> fmap (fmap handleError) (r (def {readerApplyMacros = True}))
                            _                     -> error $ "Pandoc reader '" ++ reader ++ "' not found."
defaultReaders _        = error $ "Only default Pandoc readers supported."

--- Default Tanglers
--- ----------------

defaultTanglers :: [String] -> Pandoc -> Pandoc
defaultTanglers ["id"]           = id
defaultTanglers ["code"]         = dropSectWithoutCode
defaultTanglers ("code" : codes) = dropSectWithoutCode . takeCodes codes
defaultTanglers tangler          = error $ "Tangler '" ++ intercalate " " tangler ++ "' not found."

--- Default Writers
--- ---------------

defaultWriters :: [String] -> Pandoc -> String
defaultWriters ["pandoc", writer]
    = case lookup writer writers of
        Just (PureStringWriter w) -> w (def {writerColumns = 80})
        _                         -> error $ "Pandoc writer '" ++ writer ++ "' not found."
defaultWriters ["code", lang]
    = intercalate "\n" . concatMap (writeCodeBlock lang) . getBlocks . dropSectWithoutCode . takeCode lang
defaultWriters w
    = error $ "Writer '" ++ intercalate " " w ++ "' not found."

writeCodeBlock :: String -> Block -> [String]
writeCodeBlock lang (CodeBlock (_,ls,_) code)
    | lang `elem` ls = "" : lines code
writeCodeBlock lang h@(Header n _ _)
    = let writeMD = defaultWriters ["pandoc", "markdown"]
          comment = map (commentL lang ++) . lines . writeMD . Pandoc nullMeta $ [h]
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
writeCodeBlock _ _         = []
