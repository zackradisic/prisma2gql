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
import Opt (Options (optsOutputPath, optsPrismaSchemaPath, optsTemplatePath), parseOptions, parserInfo)
import Options.Applicative (execParser)
import Parser (schema)
import System.Directory.Extra (doesFileExist, removeFile)
import System.Environment (getArgs)
import Text.Megaparsec (parseTest, runParser)
import Text.Pretty.Simple (pPrint)
import Types

main :: IO ()
main = do
  opts <- execParser parserInfo
  _ <- printOpt opts
  prismaFileStr <- readFile (cs $ optsPrismaSchemaPath opts)
  let prismaFileTxt = pack prismaFileStr
  templateFileStr <- maybe (pure "") (readTemplateFile . cs) (optsTemplatePath opts)

  case runParser schema "Test" prismaFileTxt of
    Left e -> do
      putStrLn "Failed to parse schema, is it a syntactically correct Prisma schema file?"
      print e
    Right schema -> do
      s <-
        runStateT (setupGenerate *> generate schema) $
          newCodegenEnv
            (cs $ optsOutputPath opts)
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

printOpt :: Options -> IO ()
printOpt opts =
  putStrLn $
    "Running with args: \n"
      ++ "    Prisma schema path: "
      ++ (cs $ optsPrismaSchemaPath opts)
      ++ "\n    Template path: "
      ++ maybe "Nothing" cs (optsTemplatePath opts)
      ++ "\n    Output path: "
      ++ cs (optsOutputPath opts)

readTemplateFile :: FilePath -> IO Text
readTemplateFile filePath = do
  exists <- doesFileExist filePath
  if exists
    then pack <$> readFile filePath
    else pure ""
