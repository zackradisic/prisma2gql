{-# LANGUAGE OverloadedStrings #-}

module Opt where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Options.Applicative

data Options = Options
  { optsPrismaSchemaPath :: Text,
    optsTemplatePath :: Maybe Text,
    optsOutputPath :: Text
  }
  deriving (Show, Eq)

defaultOptions :: Maybe Text -> Maybe Text -> Maybe Text -> Options
defaultOptions prismaPath templatePath outputPath =
  Options
    (fromMaybe "./schema.prisma" prismaPath)
    templatePath
    (fromMaybe "./schema.generated.gql" outputPath)

parseOptions :: Parser Options
parseOptions =
  defaultOptions
    <$> optional (strOption (long "schema" <> help "Path to Prisma schema file"))
    <*> optional (strOption (long "template" <> help "Path to template"))
    <*> optional (strOption (long "out" <> help "Path for output generate gql schema"))

parserInfo :: ParserInfo Options
parserInfo =
  info
    (parseOptions <**> helper)
    ( fullDesc <> progDesc "Generate a GraphQL schema from a Prisma schema" <> header "prisma2gql - Prisma -> GraphQl generate"
    )
