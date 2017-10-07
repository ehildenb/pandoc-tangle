--- Tangle
--- ======

--- This module can be used to aid tangling documents in Pandoc's internal
--- representation. Metadata can be used to inform the derivation process, and
--- various small document manipulators are supplied.


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


--- Document Manipulation
--- ---------------------

--- These functions are of type `Pandoc -> Pandoc`, so they can be used to build up
--- document manipulations using function composition.

--- ### Take Functions

--- In general, the `take*` functions will look for sections that have certain
--- properties and keep those sections along with all super-sections. Super-sections
--- text which is not part of one of it's sub-sections will be kept as well.

--- The `takeSects` function will only keep sections of the document which are in
--- the supplied list of section names. It will also take super-sections of any
--- section that is kept.


takeSects :: [[Inline]] -> Pandoc -> Pandoc
takeSects names (Pandoc m bs) = Pandoc m $ takeSectWith (sectNames names) bs


--- `takeSectWithCode` will only take sections which have codeblocks in them. This
--- is useful for creating a minimal "skeleton" document to write a code-file from.


takeSectWithCode :: Pandoc -> Pandoc
takeSectWithCode (Pandoc m bs) = Pandoc m $ takeSectWith (any codeBlock) bs


--- `takeSectWith` accepts a predicate over a section (so a predicate over the list
--- of blocks that make up a section) and keeps sections which match that predicate.
--- Note that the predicate is run *only* over the section header and the
--- section-text, *not* including the sub-section headers or text of that section.
--- If any of the sub-sections of a section are kept, the section is kept as well.


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


--- `takeCodes` will only keep code marked with the specified classes.


takeCodes :: [String] -> Pandoc -> Pandoc
takeCodes codes = walk (onlyCodeClasses codes)

onlyCodeClass :: String -> Block -> Block
onlyCodeClass code b = if codeBlock `implies` isClass code $ b then b else Null

onlyCodeClasses :: [String] -> Block -> Block
onlyCodeClasses codes b = if codeBlock `implies` hasClass codes $ b then b else Null


--- `onlyCode` will keep level 3 or lower headers, along with codeblocks.


onlyCode :: Pandoc -> Pandoc
onlyCode = walk onlyCode'
    where
        onlyCode' h@(Header n _ _)
            | n <= 3                 = h
        onlyCode' cb@(CodeBlock _ _) = cb
        onlyCode' _                  = Null


--- ### Drop Functions

--- The `drop*` functions will look for sections with certain properties and drop
--- those sections and all sub-sections.

--- `dropSects` will remove all sections which have names in the supplied list of
--- section names. It uses `dropSect`, which removes only one section name from a
--- document.


dropSects :: [[Inline]] -> Pandoc -> Pandoc
dropSects = foldl (\bf n -> bf . dropSect n) id

dropSect :: [Inline] -> Pandoc -> Pandoc
dropSect name (Pandoc m bs)
    = let beforeSect = takeWhile (not . headerName name) bs
          afterSect  = case dropWhile (not . headerName name) bs of
                           []                      -> []
                           (h@(Header n _ _) : bs) -> dropWhile (not . headerN (<= n)) bs
      in  Pandoc m $ beforeSect ++ afterSect


--- `dropClasses` will remove the listed class attributes from the code-blocks and
--- headers to avoid clutter in the final document.


dropClasses :: [String] -> Block -> Block
dropClasses classes (CodeBlock (_, cs, _) code) = CodeBlock ("", cs \\ classes, []) code
dropClasses classes (Header n  (_, cs, _) h)    = Header n  ("", cs \\ classes, []) h
dropClasses classes b                           = b


--- `dropSectWithoutCode` will remove any section that contains no code.


dropSectWithoutCode :: Pandoc -> Pandoc
dropSectWithoutCode (Pandoc m bs) = Pandoc m $ takeSectWith (any codeBlock) bs


--- `dropMath` will remove all math from a document. It's useful if you have some
--- target which doesn't handle math very well.


dropMath :: Pandoc -> Pandoc
dropMath
    = let nullMath (Para [(Math _ _)]) = Null
          nullMath b                   = b
      in  walk nullMath


--- `flatDivs` will flatten all `div` in a document into their parents.


flatDivs :: Pandoc -> Pandoc
flatDivs (Pandoc m bs) = Pandoc m (walkM flatDivs' (Div nullAttr bs))
    where
        flatDivs' (Div _ bs) = bs
        flatDivs' b          = [b]


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


--- Metadata Extraction
--- -------------------


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

