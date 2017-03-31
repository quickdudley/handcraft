{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}
module Web.Html (
  HTML,
  Selector,
  html,
  runSelector,
  searchAll,
  searchFixedDepth,
  tagName,
  search,
  searchDepth,
  replace,
  exclude,
  trim,
  foreach,
  get,
  getText,
  getTextS,
  getAttributes,
  getAttribute,
  getAttributesS,
  getAttributeS,
  scratch
 ) where

import Language.Haskell.TH
import Control.Applicative
import Control.Monad
import Codec.Phaser.Core hiding (get)
import qualified Codec.Phaser.Core as P
import Codec.Phaser.Common hiding (get)
import Codec.Phaser.UTF8
import Data.Char
import Data.List

import Web.Html_stage1

data HTML =
  JustChar {-# UNPACK #-} !Char |
  Tag String [(String,String)] [HTML]
 deriving (Show)

newtype Selector a = S { runSelector :: ([HTML] -> ([HTML],a)) }

instance Functor Selector where
  fmap f (S s) = S ((fmap . fmap) f s)

instance Applicative Selector where
  pure = S . flip (,)
  S f <*> S a = S (\h1 -> let
    (h2,f') = f h1
    (h3,a') = a h2
    in (h3, f' a')
   )

instance Monad Selector where
  return = pure
  S a >>= f = S (\h1 -> let
    (h2,a') = a h1
    S b = f a'
    in b h2
   )

instance (Monoid a) => Monoid (Selector a) where
  mempty = S $ const ([],mempty)
  mappend (S a) (S b) = S $ \h -> let
    (ha,ra) = a h
    (hb,rb) = b h
    in (ha ++ hb, mappend ra rb)

rawchar = satisfy (not . flip elem "<>&")

namedEntity = $( do
  mDefs <- runIO $ parseFile (utf8_stream >># trackPosition >># entityDefs)
    "app/Web/entityref.sgml"
  case mDefs of
    Right (defs:_) ->
      [| char '&' *> $(foldr (\(n,c) r -> [| c <$ string n <|> $(r) |]) [| empty |] defs) <* char ';' |]
 )

plaintext = go id id where
  go acc wcc = r acc wcc <|> e (acc . wcc)
  r acc wcc = do
    c <- rawchar
    case c of
      '\r' -> n acc
      '\n' -> n acc
      _ | isSpace c -> go acc (wcc . (c :)) <|> pure (acc [])
      _ -> let
        acc' = acc . wcc . (c :)
        in go acc' id <|> pure (acc' [])
  e acc = do
    c <- namedEntity <|> numericReference
    let acc' = acc . (c :)
    go acc' id <|> pure (acc' [])
  n acc = (<|> e acc) $ (<|> pure (acc [])) $ do
    c <- rawchar
    if isSpace c
      then n acc
      else let acc' = (acc . (c :)) in go acc' id <|> pure (acc' [])

voidTags = [
  "area",
  "base",
  "br",
  "col",
  "hr",
  "img",
  "input",
  "link",
  "meta",
  "param",
  "keygen",
  "source"
 ]

tag = let
  openTag = (,,) <$>
    (char '<' *> munch isSpace *> munch1 nameChar <* munch isSpace) <*>
    (sepBy
      ((,) <$> munch1 nameChar <*>
        (munch isSpace *> char '=' *> munch isSpace *>
          ((char '\"' *> munch (/= '\"') <* char '\"') <|>
            (char '\'' *> munch (/= '\'') <* char '\'') <|>
            (munch1 (not . ((||) <$> isSpace <*> (`elem` "<>\"\'"))))) <|> pure ""))
      (munch1 isSpace)) <*>
    (munch isSpace *> ((True <$ char '/') <|> pure False) <*
      munch isSpace <* char '>')
  closeTag n = do
    n' <-
      char '<' *>
      munch isSpace *>
      char '/' *>
      munch1 nameChar <*
      munch isSpace <*
      char '>'
    when (map toLower n /= map toLower n') $ put $ "</" ++ n' ++ ">"
  nameChar = (||) <$> isAlphaNum <*> (`elem` ":-")
  js :: Monoid p => ([Char] -> [Char]) -> ([Char] -> [Char]) -> [Char] -> Phase p Char o [HTML]
  js acc _ [] = pure (map JustChar $ acc [])
  js acc ctp (c:r) = do
    c' <- P.get
    case () of
      _ | toLower c' == c -> js acc (ctp . (c' :)) r
      _ | c' == '<' -> js (acc . ctp) (c' :) "/script>"
      _ -> js (acc . ctp . (c' :)) id "</script>"
  in do
    (n,a,ec) <- openTag
    case () of
      _ | ec -> pure (Tag n a [])
      _ | map toLower n == "script" -> Tag n a <$> js id id "</script>"
      _ | map toLower n `elem` voidTags -> pure (Tag n a [])
      _ -> (Tag n a <$> (html <* (closeTag n <|> eof)))

html = d <|> p id <|> t id <|> pure [] where
  d = iString "<!DOCTYPE" *>
    munch (/= '>') *>
    char '>' *>
    (p id <|> t id)
  p acc = (map JustChar <$> plaintext) >>= \s -> let
    acc' = acc . (s ++)
    in t acc' <|> c acc' <|> pure (acc' [])
  t acc = tag >>= \t' -> let
    acc' = acc . (t' :)
    in p acc' <|> t acc' <|> c acc' <|> pure (acc' [])
  c acc = comment *> (p acc <|> t acc <|> c acc <|> pure (acc []))

searchAll :: (HTML -> Bool) -> Selector ()
searchAll p = S (flip (,) () . concatMap go) where
  go h1 = let
    c = case h1 of
      JustChar _ -> []
      Tag _ _ c' -> concatMap go c'
    in if p h1 then h1 : c else c

searchFixedDepth :: Int -> (HTML -> Bool) -> Selector ()
searchFixedDepth n0 p = S (flip (,) () . concatMap (flip go n0)) where
  go :: HTML -> Int -> [HTML]
  go h1 0 = if p h1 then [h1] else []
  go h1 n = let
    c = case h1 of
      JustChar _ -> []
      Tag _ _ c' -> concatMap (flip go (n - 1)) c'
    in if p h1 then h1 : c else c

search :: (HTML -> Bool) -> Selector ()
search p = S (flip (,) () . concatMap go) where
  go h1 = let
    c = case h1 of
      JustChar _ -> []
      Tag _ _ c' -> concatMap go c'
    in if p h1 then [h1] else c

searchDepth :: Int -> (HTML -> Bool) -> Selector ()
searchDepth n0 p = S (flip (,) () . concatMap (flip go n0)) where
  go h1 0 = if p h1 then [h1] else []
  go h1 n = let
    c = case h1 of
      JustChar _ -> []
      Tag _ _ c' -> concatMap (flip go (n - 1)) c'
    in if p h1 then [h1] else c

replace p t = S $ \h -> (concatMap go h, ()) where
  go x@(JustChar _) = if p x then r else [x]
  go x@(Tag n a c) = if p x then r else [Tag n a $ concatMap go c]
  r = map JustChar t

get :: Selector [HTML]
get = S $ join (,)

exclude :: (HTML -> Bool) -> Selector ()
exclude p = S (flip (,) () . concatMap go) where
  go x@(JustChar _) = if p x then [] else [x]
  go x@(Tag n a c) = if p x
    then []
    else [Tag n a (concatMap go c)]

trim :: Int -> Selector ()
trim n0 = S (\h -> (concatMap (go n0) h,())) where
  go _ x@(JustChar _) = [x]
  go 0 _ = []
  go n (Tag l a c) = [Tag l a (concatMap (go (n - 1)) c)]

foreach :: Selector a -> Selector [a]
foreach (S a) = S
  (uncurry (flip (,)) . fmap concat . uncurry (flip (,)) . unzip . map a . gr)
 where
  gr = gr' id where
    gr' acc [] = acc []
    gr' acc (t@(Tag _ _ _):r) = gr' (acc . ([t]:)) r
    gr' acc r = grc acc id r
    grc accm accc (c@(JustChar _):r) = grc accm (accc . (c :)) r
    grc accm accc r = gr' (accm . (accc [] :)) r

tagName :: HTML -> Maybe String
tagName (JustChar _) = Nothing
tagName (Tag n _ _) = Just n

getText :: HTML -> String
getText (JustChar c) = [c]
getText (Tag _ _ c) = c >>= getText

getTextS :: Selector String
getTextS = S (\h -> (h, h >>= getText))

getAttributes :: HTML -> [(String,String)]
getAttributes (JustChar _) = []
getAttributes (Tag _ a _) = a

getAttribute :: String -> HTML -> Maybe String
getAttribute _ (JustChar _) = Nothing
getAttribute n (Tag _ a _) = lookup n a

getAttributesS :: Selector [(String,String)]
getAttributesS = S (\h -> (h, h >>= getAttributes))

getAttributeS n = S (\h -> (h, foldr (<|>) Nothing $ map (getAttribute n) h))

scratch :: Selector a -> Selector a
scratch (S a) = S (\h -> let (_,v) = a h in (h,v))
