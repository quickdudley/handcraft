{-# LANGUAGE NoMonomorphismRestriction #-}
module Web.Html_stage1 (
  numericReference,
  entityDefs,
  comment
 ) where

import Control.Applicative
import Control.Monad
import Codec.Phaser.Core
import Codec.Phaser.Common
import Data.Char

numericReference :: (Monoid p) => Phase p Char o Char
numericReference = string "&#" *> (toEnum <$> nr) <* char ';' where
  nr = positiveIntegerDecimal <|> (iChar 'x' *> directHex)

comment :: (Monoid p) => Phase p Char o ()
comment = string "<!--" *> go "-->" where
  go [] = return ()
  go (a:r) = get >>= \c -> if c == a
    then go r
    else go "-->"

entityDef :: (Monoid p) => Phase p Char o (String, Char)
entityDef = (,) <$>
  (string "<!ENTITY" *> munch1 isSpace *> munch1 isAlphaNum) <*>
  (munch1 isSpace *> string "CDATA" *> munch1 isSpace *> char '\"' *>
    numericReference <* char '\"' <* munch isSpace <* string "--" <* go ">"
   )
 where
  go [] = return ()
  go (a:r) = get >>= \c -> if c == a
    then go r
    else go "-->"

entityDefs :: (Monoid p) => Phase p Char o [(String,Char)]
entityDefs = sepBy entityDef
  (void (many (void (munch1 isSpace) <|> comment)) <|> return ()) <*
  munch isSpace
