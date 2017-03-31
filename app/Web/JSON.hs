{-# LANGUAGE NoMonomorphismRestriction #-}
module Web.JSON (
  JSON(..),
  jsonDocument
 ) where

import Data.Bits
import Data.Char

import Control.Applicative
import Control.Monad

import Codec.Phaser.Core
import Codec.Phaser.Common
import Codec.Phaser.UTF16

data JSON = JSONArray [JSON] |
  JSONObject [(String,JSON)] |
  JSONString String |
  JSONNumber Rational |
  JSONBool Bool |
  JSONNull
 deriving (Show)

jsonNumber = (pure id <|> (negate <$ char '-')) <*>
  ((\i m e -> (i + m) * 10 ^ e) <$>
    positiveIntegerDecimal <*>
    (pure 0 <|> (char '.' *> m 0.1 0.0)) <*>
    (pure 0 <|> (
      (iChar 'e' *> (pure id <|> (id <$ char '+') <|> (negate <$ char '-'))) <*>
      positiveIntegerDecimal
     ))
   )
 where
  m s t = do
    d <- (fromIntegral . digitToInt) <$> satisfy isDigit
    let t' = t + s * d
    t' `seq` pure t' <|> m (s / 10) t'

jsonBool = (True <$ string "true") <|> (False <$ string "false")

jsonString = char '\"' *> many sc <* char '\"' where
  sc = satisfy ((&&) <$> (/= '\\') <*> (/= '\"')) <|> esc
  esc = char '\\' >> get >>= \c -> case c of
    '\"' -> pure '\"'
    '/' -> pure '/'
    'b' -> pure '\b'
    'f' -> pure '\f'
    'n' -> pure '\n'
    'r' -> pure '\r'
    't' -> pure '\t'
    'u' -> fromAutomaton $ sepBy (ch4 >>= yield) (string "\\u") >># utf16_char
  hd = (fromIntegral . digitToInt) <$> satisfy isHexDigit
  ch4 = (\a b c d ->
    shiftL a 12 .|.
    shiftL b 8 .|.
    shiftL c 4 .|.
    d
   ) <$> hd <*> hd <*> hd <*> hd

jsonArray = (char '[' <* munch isSpace) *> ac <* char ']' where
  ac = sepBy (jsonValue <* munch isSpace) (char ',' <* munch isSpace)

jsonObject = (char '{' <* munch isSpace) *> oc <* char '}' where
  oc = sepBy (pair <* munch isSpace) (char ',' <* munch isSpace)
  pair = (,) <$> jsonString <*>
    (munch isSpace *> char ':' *> munch isSpace *> jsonValue <* munch isSpace)

jsonDocument' = (JSONArray <$> jsonArray) <|> (JSONObject <$> jsonObject)

jsonValue = jsonDocument' <|>
  (JSONString <$> jsonString) <|>
  (JSONNumber <$> jsonNumber) <|>
  (JSONBool <$> jsonBool) <|>
  (JSONNull <$ string "null")

jsonDocument = munch isSpace *> jsonDocument' <* munch isSpace
