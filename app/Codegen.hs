{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- |
module Codegen where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (Reader, ReaderT (runReaderT), ask, reader, runReader)
import Control.Monad.Trans.State.Lazy (State, StateT, get, modify, runState, state)
import Control.Monad.Trans.Writer (WriterT, tell)
import Data.ByteString (ByteString, appendFile)
import Data.Char (toLower, toUpper)
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity)
import Data.List (filter, find)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace (traceShow)
import GHC.Base (when)
import Types
import Prelude hiding (Enum, appendFile, log)

data ModelIgnore = ModelIgnore [Text] deriving (Eq, Show)

data CodegenEnvT = CodegenEnv
  { codegenFilePath :: FilePath,
    codegenIndentation :: Int,
    codegenImports :: Text,
    codegenModelIgnore :: ModelIgnore,
    codegenLines :: [ByteString],
    codegenShouldIgnoreRelations :: Bool
  }
  deriving (Eq, Show)

newCodegenEnv :: FilePath -> Text -> [ByteString] -> CodegenEnvT
newCodegenEnv filePath imports lines =
  CodegenEnv
    { codegenFilePath = filePath,
      codegenIndentation = 0,
      codegenImports = imports <> "scalar DateTime\nscalar Byte\nscalar BigInt\n",
      codegenModelIgnore = ModelIgnore [],
      codegenLines = lines,
      codegenShouldIgnoreRelations = False
    }

type CodegenState m = StateT CodegenEnvT m

write :: ByteString -> CodegenState IO ()
write str = do
  indentation <- getIndentation
  get >>= \s -> liftIO $ appendFile (codegenFilePath s) (indentation <> str)

writeLn :: ByteString -> CodegenState IO ()
writeLn str = write (str <> "\n")

getIndentation :: Monad m => CodegenState m ByteString
getIndentation = get <&> (indentStr "" . codegenIndentation)
  where
    indentStr :: ByteString -> Int -> ByteString
    indentStr s count =
      if count == 0
        then ""
        else " " <> indentStr s (count - 1)

shouldIgnoreField :: Monad m => Text -> CodegenState m Bool
shouldIgnoreField field = do
  ModelIgnore ignoredFields <- codegenModelIgnore <$> get
  pure $ elem field ignoredFields

findIgnored :: [Annotation] -> [Text]
findIgnored [] = []
findIgnored ((Annotation ("relation", Just (Labeled args))) : xs) =
  case find (\(name, val) -> name == "fields") args of
    Just (_, value) -> case value of
      ArrValue ignoredFieldNames ->
        foldr
          ( \value xs -> case value of
              IdentValue name -> name : xs
              _ -> xs
          )
          []
          ignoredFieldNames
          <> findIgnored xs
      _ -> findIgnored xs
    Nothing -> findIgnored xs
findIgnored (x : xs) = findIgnored xs

appendIgnored :: Monad m => [Text] -> CodegenState m ()
appendIgnored ignored =
  modify
    ( \s ->
        s
          { codegenModelIgnore =
              let ModelIgnore current = codegenModelIgnore s
               in ModelIgnore (current <> ignored)
          }
    )

resetIgnored :: Monad m => CodegenState m ()
resetIgnored =
  modify (\s -> s {codegenModelIgnore = ModelIgnore []})

indent :: Monad m => CodegenState m ()
indent = modify (\s -> s {codegenIndentation = codegenIndentation s + 4})

dedent :: Monad m => CodegenState m ()
dedent =
  modify
    ( \s ->
        s
          { codegenIndentation = case codegenIndentation s of
              0 -> 0
              x -> x - 4
          }
    )

withIndent :: ByteString -> CodegenState IO ()
withIndent str = do
  _ <- indent
  _ <- write (str <> "\n")
  dedent

withIndentFull :: ByteString -> CodegenState IO () -> ByteString -> CodegenState IO ()
withIndentFull start mid end = do
  _ <- writeLn start
  _ <- indent
  _ <- mid
  _ <- dedent
  writeLn end

generate :: [Decl] -> CodegenState IO ()
generate [] = pure ()
generate ((ModelDecl m) : xs) = do
  generateModel m
  generate xs
generate ((EnumDecl e) : xs) = do
  generateEnum e
  generate xs
generate (_ : xs) = generate xs

generateEnum :: Enum -> CodegenState IO ()
generateEnum (Enum (name, variants)) = do
  withIndentFull
    (cs $ "enum " <> name <> " {")
    ( foldl
        (\sum field -> sum >> writeLn (cs field))
        (pure ())
        variants
    )
    "}\n"

generateModel :: Model -> CodegenState IO ()
generateModel (Model (name, fields)) = do
  withIndentFull
    (cs $ "type " <> name <> " {")
    ( foldl
        (\sum (idx, field) -> sum >> generateField name field)
        (pure ())
        (zip [0 ..] fields)
    )
    "}\n"
  resetIgnored

generateField :: Text -> Field -> CodegenState IO ()
generateField modelName f = do
  shouldIgnoreRelations <- codegenShouldIgnoreRelations <$> get
  if shouldIgnoreRelations
    then case hasRelation f of
      Just annotation -> do
        appendIgnored (findIgnored annotation)
        pure ()
      Nothing -> do
        shouldIgnore <- shouldIgnoreField (fieldName f)
        unless shouldIgnore writeField
    else writeField
  where
    writeField = do
      lines <- codegenLines <$> get
      let isIgnored = hasIgnoreComment f lines
      unless
        isIgnored
        ( let isOptional = hasOptionalComment f lines
           in writeLn (cs (fieldName f) <> ": " <> generateType (fieldType f) (not isOptional))
        )

generateType :: Type -> Bool -> ByteString
generateType (OptionalTy ty) _ = generateType ty False
generateType (NamedTy name) required = generateRequired (cs name) required
generateType (ArrayTy ty) required = generateRequired ("[" <> generateType ty True <> "]") required
generateType (KeywordTy IntTy) required = generateRequired "Int" required
generateType (KeywordTy DateTimeTy) required = generateRequired "UTCTime" required
generateType (KeywordTy StringTy) required = generateRequired "Text" required
generateType (KeywordTy BytesTy) required = generateRequired "Byte" required
generateType (KeywordTy BigIntTy) required = generateRequired "BigInt" required

generateRequired :: ByteString -> Bool -> ByteString
generateRequired str required = if required then str <> "!" else str

modelDependencies :: Model -> [Model] -> [Text]
modelDependencies (Model (_, [])) models = []
modelDependencies (Model (_, xs)) models = findDependencies xs
  where
    findDependencies :: [Field] -> [Text]
    findDependencies [] = []
    findDependencies (x : xs) = maybe (findDependencies xs) (flip (:) $ findDependencies xs) (dependenciesFromType $ fieldType x)

dependenciesFromType :: Type -> Maybe Text
dependenciesFromType (OptionalTy ty) = dependenciesFromType ty
dependenciesFromType (KeywordTy _) = Nothing
dependenciesFromType (NamedTy ident) = Just ident
dependenciesFromType (ArrayTy ty) = dependenciesFromType ty
