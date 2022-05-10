{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Codegen (CodegenEnvT (codegenFilePath, codegenImports, codegenLines), CodegenState, generate, generateModel, newCodegenEnv, write)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Lazy (StateT (runStateT), get, modify)
import Data.List (find)
import Data.List.Extra ((!?))
import Data.Maybe (fromMaybe, isJust)
import Data.String.Conversions (cs)
import Data.Text (Text, pack, replace, unpack)
import Parser (schema)
import System.Directory.Extra (doesFileExist, removeFile)
import System.Environment (getArgs)
import Text.Megaparsec (parseTest, runParser)
import Text.Pretty.Simple (pPrint)
import Types

type CliArgs = (Maybe String, Maybe String, Maybe String)

argsPrismaSchemaPath :: CliArgs -> String
argsPrismaSchemaPath (_, path, _) = fromMaybe "./schema.prisma" path

argsTemplatePath :: CliArgs -> String
argsTemplatePath (path, _, _) = fromMaybe "./template.gql" path

argsOutputPath :: CliArgs -> String
argsOutputPath (_, _, path) = fromMaybe "./schema.geneated.gql" path

main :: IO ()
main = do
  args <- parseArgs
  printArgs args
  prismaFileStr <- readFile (argsPrismaSchemaPath args)
  let prismaFileTxt = pack prismaFileStr
  templateFileStr <- readTemplateFile (argsTemplatePath args)

  case runParser schema "Test" prismaFileTxt of
    Left e -> do
      putStrLn "Failed to parse schema, is it a syntactically correct Prisma schema file?"
      print e
    Right schema -> do
      s <-
        runStateT (setupGenerate *> generate schema) $
          newCodegenEnv
            (argsOutputPath args)
            templateFileStr
            (cs <$> lines prismaFileStr)
      putStrLn "Success!"
      pure ()

setupGenerate :: CodegenState IO ()
setupGenerate = deleteIfExists *> writeImports
  where
    deleteIfExists = do
      filePath <- codegenFilePath <$> get
      exists <- liftIO $ doesFileExist filePath
      if exists
        then do
          liftIO $ putStrLn "Output Prisma file already exists, overwriting..."
          liftIO $ removeFile filePath
        else pure ()
    writeImports = do
      liftIO $ putStrLn "Writing template..."
      s <- get
      liftIO $ writeFile (codegenFilePath s) $ cs (codegenImports s) ++ "\n\n"

readTemplateFile :: FilePath -> IO Text
readTemplateFile filePath = do
  exists <- doesFileExist filePath
  if exists
    then pack <$> readFile filePath
    else pure ""

printArgs :: CliArgs -> IO ()
printArgs args = do
  putStrLn "Running with the following configuration..."
  putStrLn ("    Prisma Schema Path: " ++ argsPrismaSchemaPath args)
  putStrLn ("    Template Path: " ++ argsTemplatePath args)
  putStrLn ("    Output Path: " ++ argsOutputPath args)
  pure ()

parseArgs :: IO CliArgs
parseArgs = do
  args <- getArgs
  pure (args !? 0, args !? 1, args !? 2)
