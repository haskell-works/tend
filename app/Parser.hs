
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Parser where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text as AT
import Data.Char
import Data.List.Extra
import Data.Maybe
import Data.Text
import qualified Parser.Types as T

(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) f g c = f c || g c

isHyphen :: Char -> Bool
isHyphen c = c == '-'

isPeriod :: Char -> Bool
isPeriod c = c == '.'

isUnderscore :: Char -> Bool
isUnderscore c = c == '~'

isTilde :: Char -> Bool
isTilde c = c == '~'

alphaNum :: Parser Char
alphaNum = satisfy isAlphaNum

isUnreserved :: Char -> Bool
isUnreserved = isAlpha ||| isDigit ||| isHyphen ||| isPeriod ||| isUnderscore ||| isTilde

unreserved :: Parser Char
unreserved = satisfy isUnreserved

unreserved_0 :: Parser Text
unreserved_0 = pack <$> many unreserved

unreserved_1 :: Parser Text
unreserved_1 = pack <$> many unreserved

organisation :: Parser Text
organisation = pack <$> many (satisfy (/= '/'))

user :: Parser T.User
user = T.User <$> unreserved_1

host :: Parser T.Host
host = T.Host <$> unreserved_1

authority :: Parser T.Authority
authority = T.Authority <$> optional user <*> host

pchar :: Parser Char
pchar = unreserved <|> char ':' <|> char '@'

segment :: Parser T.Segment
segment = T.Segment <$> unreserved_1
