{-# LANGUAGE OverloadedStrings #-}

module Parser (schema) where

import Control.Applicative (Alternative (many), liftA2, liftA3)
import Data.Either
import Data.Functor ((<&>))
import Data.Text (Text)
import Debug.Trace (traceShow)
import Scanner
import Text.Megaparsec hiding (many)
import Text.Megaparsec.Byte (string)
import Text.Megaparsec.Debug (dbg)
import Types
import Prelude hiding (Enum)

schema :: Parser [Decl]
schema = sc *> many declP -- <* eof

declP :: Parser Decl
declP =
  choice
    [ GeneratorDecl <$> generatorP,
      DatasourceDecl <$> datasourceP,
      ModelDecl <$> modelP,
      EnumDecl <$> enumP
    ]

enumP :: Parser Enum
enumP = do
  rword "enum"
  ident <- identifier
  variants <- curlyBraces (many identifier)
  pure $ Enum (ident, variants)

modelP :: Parser Model
modelP = do
  rword "model"
  ident <- identifier
  fields <- curlyBraces (many fieldP)
  pure $ Model (ident, fields)

fieldP :: Parser Field
fieldP =
  lexeme $ do
    pos <- getSourcePos
    name <- identifier
    ty <- tyP
    annotations <- many (sc *> annotationP)
    pure $
      Field
        { fieldName = name,
          fieldType = ty,
          fieldAnnotations = annotations,
          fieldPos = pos
        }

annotationP :: Parser Annotation
annotationP =
  lexeme $ do
    anno <- annotation
    values <- option Nothing (parensNoSkipSpace annotationArgsP <&> Just)
    pure $ Annotation (anno, values)

annotationArgsP :: Parser Args
annotationArgsP =
  -- `sepBy1` is important here, because `sepBy` parses ZERO or more, meaning it doesn't fail if it gets no matches. Wasted like 15 minutes
  try (labeledArgP `sepBy1` comma <&> Labeled)
    <|> (Unlabeled <$> valP `sepBy` comma)

labeledArgP :: Parser (Text, Value)
labeledArgP = do
  ident <- identifier
  _ <- symbol ":"
  val <- valP
  pure (ident, val)

tyP :: Parser Type
tyP = lexeme optionalTyP

optionalTyP :: Parser Type
optionalTyP =
  (lexeme . choice)
    [ try _optionalTyP,
      arrTyP
    ]

_optionalTyP :: Parser Type
_optionalTyP = try $ do
  ty <- nonOptionalTyP
  symbol "?"
  pure $ OptionalTy ty

nonOptionalTyP :: Parser Type
nonOptionalTyP = lexeme $ choice [KeywordTy <$> keywordTyP, NamedTy <$> identifier]

arrTyP :: Parser Type
arrTyP =
  (lexeme . choice)
    [ _arrTyP,
      nonArrTyP
    ]

_arrTyP :: Parser Type
_arrTyP = try $ do
  ty <- nonArrTyP
  symbol "[" *> symbol "]"
  pure $ ArrayTy ty

nonArrTyP :: Parser Type
nonArrTyP = lexeme $ choice [KeywordTy <$> keywordTyP, NamedTy <$> identifier]

keywordTyP :: Parser KeywordType
keywordTyP =
  choice
    [ IntTy <$ string "Int",
      BigIntTy <$ string "BigInt",
      DateTimeTy <$ string "DateTime",
      StringTy <$ string "String",
      BytesTy <$ string "Bytes"
    ]

generatorP :: Parser Generator
generatorP = do
  rword "generator"
  ident <- identifier
  keyVals <- curlyBraces (many keyValP)
  pure $ Generator (ident, keyVals)

datasourceP :: Parser DataSource
datasourceP = do
  rword "datasource"
  ident <- identifier
  keyVals <- curlyBraces (many keyValP)
  pure $ DataSource (ident, keyVals)

keyValP :: Parser KeyVal
keyValP = do
  key <- identifier
  val <- symbol "=" *> valP
  pure $ KeyVal (key, val)

valP :: Parser Value
valP =
  (lexeme . choice)
    [ StrValue <$> strlit,
      CallValue <$> try callP,
      ArrValue <$> arrP,
      IdentValue <$> identifier
    ]

callP :: Parser Call
callP = do
  ident <- identifier
  values <-
    ( \values ->
        if null values
          then Nothing
          else Just values
      )
      <$> parens (valP `sepBy` comma)
  pure $ Call (ident, values)

arrP :: Parser [Value]
arrP = braces (many valP)
