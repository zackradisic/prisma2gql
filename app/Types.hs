{-# LANGUAGE OverloadedStrings #-}

-- |
module Types where

import Control.Monad.Trans.State.Lazy (State)
import Data.ByteString (ByteString, isInfixOf)
import Data.Functor.Identity (Identity)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Debug.Trace (traceShow)
import Text.Megaparsec (ParsecT, SourcePos (sourceLine))
import Text.Megaparsec.Pos (unPos)
import Prelude hiding (Enum)

-- type CommentState = State

data Comment = Comment

type Parser = ParsecT Void Text Identity

data Decl
  = GeneratorDecl Generator
  | DatasourceDecl DataSource
  | ModelDecl Model
  | EnumDecl Enum
  deriving (Eq, Show)

newtype Generator
  = Generator (Text, [KeyVal])
  deriving (Eq, Show)

newtype DataSource
  = DataSource (Text, [KeyVal])
  deriving (Eq, Show)

newtype Enum
  = Enum (Text, [Text])
  deriving (Eq, Show)

newtype KeyVal
  = KeyVal (Text, Value)
  deriving (Eq, Show)

data Value
  = StrValue Text
  | CallValue Call
  | ArrValue [Value]
  | IdentValue Text
  deriving (Eq, Show)

newtype Call
  = Call (Text, Maybe [Value])
  deriving (Eq, Show)

newtype Model
  = Model (Text, [Field])
  deriving (Eq, Show)

data Field = Field
  { fieldName :: Text,
    fieldType :: Type,
    fieldAnnotations :: [Annotation],
    fieldPos :: SourcePos
  }
  deriving (Eq, Show)

newtype Annotation
  = Annotation (Text, Maybe Args)
  deriving (Eq, Show)

data Args
  = Unlabeled [Value]
  | Labeled [(Text, Value)]
  deriving (Eq, Show)

data Type
  = KeywordTy KeywordType
  | NamedTy Text
  | ArrayTy Type
  | OptionalTy Type
  deriving (Eq, Show)

data KeywordType
  = IntTy
  | BigIntTy
  | DateTimeTy
  | StringTy
  | BytesTy
  deriving (Eq, Show)

modelName :: Model -> Text
modelName (Model (name, _)) = name

hasRelation :: Field -> Maybe [Annotation]
hasRelation (Field _ _ annotations _) =
  findAll annotations (\(Annotation (name, _)) -> name == "relation")

findAll :: [a] -> (a -> Bool) -> Maybe [a]
findAll [] f = Nothing
findAll xs f =
  let found = findAllImpl xs f
   in if null found
        then Nothing
        else Just found
  where
    findAllImpl :: [a] -> (a -> Bool) -> [a]
    findAllImpl [] f = []
    findAllImpl (x : xs) f =
      if f x
        then x : findAllImpl xs f
        else findAllImpl xs f

hasCommentAbove :: Field -> [ByteString] -> Bool
hasCommentAbove (Field _ _ _ pos) lines =
  let line = unPos . sourceLine $ pos
   in line > 1 && isInfixOf "// " (lines !! (line - 2))

hasOptionalComment :: Field -> [ByteString] -> Bool
hasOptionalComment field@(Field _ _ _ pos) lines =
  hasCommentAbove field lines
    && let line = unPos . sourceLine $ pos in line > 1 && isInfixOf "@optional" (lines !! (line - 2))

hasIgnoreComment :: Field -> [ByteString] -> Bool
hasIgnoreComment field@(Field _ _ _ pos) lines =
  hasCommentAbove field lines
    && let line = unPos . sourceLine $ pos in line > 1 && isInfixOf "@ignore" (lines !! (line - 2))
