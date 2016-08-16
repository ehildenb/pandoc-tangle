module Text.Pandoc.Tangle where

import Data.List ((\\))
import qualified Data.Map as M (Map, lookup, toList)

import Text.Pandoc ( Pandoc(Pandoc), Block(CodeBlock, Header, Null), Inline(Str, Space, Code, Math)
                   , Attr, nullAttr
                   , Meta(Meta), MetaValue(MetaMap, MetaInlines, MetaList, MetaString)
                   )
import Text.Pandoc.Walk (walk)

--- Document Manipulation
--- ---------------------

--- simple use of `splitSectWith`
dropSectWithoutCode :: Pandoc -> Pandoc
dropSectWithoutCode (Pandoc m bs) = Pandoc m $ takeSectWith (any codeBlock) bs

--- take sections in list of names (and sub-sections/super-sections)
takeSects :: [[Inline]] -> Pandoc -> Pandoc
takeSects names (Pandoc m bs) = Pandoc m $ takeSectWith (sectNames names) bs

--- take sections which match a predicate (including super-sections if
--- sub-section matches the predicate)
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

--- drop sections by name
dropSects :: [[Inline]] -> Pandoc -> Pandoc
dropSects = foldl (\bf n -> bf . dropSect n) id

dropSect :: [Inline] -> Pandoc -> Pandoc
dropSect name (Pandoc m bs)
    = let beforeSect = takeWhile (not . headerName name) bs
          afterSect  = case dropWhile (not . headerName name) bs of
                           []                      -> []
                           (h@(Header n _ _) : bs) -> dropWhile (not . headerN (<= n)) bs
      in  Pandoc m $ beforeSect ++ afterSect

--- drop tags from codeblocks/headers (avoids clutter in final document)
dropClasses :: [String] -> Block -> Block
dropClasses classes (CodeBlock (_, cs, _) code) = CodeBlock ("", cs \\ classes, []) code
dropClasses classes (Header n  (_, cs, _) h)    = Header n  ("", cs \\ classes, []) h
dropClasses classes b                           = b

--- CodeBlock manipulation
takeCode :: String -> Pandoc -> Pandoc
takeCode code = walk (onlyCodeClass code)

takeCodes :: [String] -> Pandoc -> Pandoc
takeCodes codes = walk (onlyCodeClasses codes)

onlyCodeClass :: String -> Block -> Block
onlyCodeClass code b = if codeBlock `implies` isClass code $ b then b else Null

onlyCodeClasses :: [String] -> Block -> Block
onlyCodeClasses codes b = if codeBlock `implies` hasClass codes $ b then b else Null

takeCodeClass :: String -> [Block] -> [Block]
takeCodeClass str = filter (codeBlock `implies` isClass str)

--- remove math blocks
removeMath :: Inline -> Inline
removeMath (Math _ _) = Space
removeMath i          = i

--- use predicate over blocks to filter document
onlyBlocks :: (Block -> Bool) -> Pandoc -> Pandoc
onlyBlocks p (Pandoc m bs) = Pandoc m $ filter p bs

--- get the blocks out of a document
getBlocks :: Pandoc -> [Block]
getBlocks (Pandoc _ bs) = bs

--- Predicates over Documents
--- -------------------------

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

--- Meta-data Extraction
--- --------------------

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
