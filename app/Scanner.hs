{-# LANGUAGE OverloadedStrings #-}

module Scanner where

import Control.Applicative (Alternative (many), some, (<|>))
import Control.Monad (void)
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Debug.Trace (traceShow)
import Text.Megaparsec hiding (many, some)
import Text.Megaparsec.Byte (string)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug (dbg)
import Types

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

parensNoSkipSpace :: Parser a -> Parser a
parensNoSkipSpace = between (L.symbol (pure ()) "(") (L.symbol (pure ()) ")")

curlyBraces :: Parser a -> Parser a
curlyBraces = between (symbol "{") (symbol "}")

braces :: Parser a -> Parser a
braces = between (symbol "[") (symbol "]")

quotes :: Parser a -> Parser a
quotes = between (single '"') (single '"')

comma :: Parser ()
comma = void $ symbol ","

rword :: Text -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

rwords :: [Text]
rwords = ["Int", "DateTime", "String", "unique"]

annotation :: Parser Text
annotation = (lexeme . try) (string "@" *> identifier >>= check)
  where
    check x =
      if x `elem` annotationIdents
        then return x
        else fail $ show x <> " is not an annotation"

annotationIdents :: [Text]
annotationIdents = ["id", "default", "relation", "unique", "map"]

strlit :: Parser Text
strlit = do
  content <- quotes $ takeWhileP Nothing (/= '"')
  pure $ T.pack ('"' : T.unpack content ++ "\"")

identifier :: Parser Text
identifier = identifierImpl False

identifierImpl :: Bool -> Parser Text
identifierImpl isReserved = (lexeme . try) (p >>= check isReserved)
  where
    p =
      fmap T.pack $
        (:) <$> letterChar
          <*> many (alphaNumChar <|> single '_')
    check shouldCheck x =
      if shouldCheck && x `elem` rwords
        then fail $ "keyword " <> show x <> " cannot be an identifier"
        else return x
