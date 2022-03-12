{-# LANGUAGE ApplicativeDo #-}
module Conf.Parse
  ( confParser
  , confParserInfo
  ) where

import           Conf.Types
import           Options.Applicative

filesystemConfParser :: Parser FilesystemConf
filesystemConfParser = do
  _fcOutputDir      <- dirParser "output" 'D'
  pure FilesystemConf { .. }

confParser :: Parser Conf
confParser = Conf <$> filesystemConfParser

dirParser name short' = strOption
  (long long' <> short short' <> help helpText <> metavar "DIRECTORY")
 where
  helpText =
    name
      <> " directory under which to construct the tree, existing files will be overwritten."
  long' = name <> "-dir"

confParserInfo :: ParserInfo Conf
confParserInfo = info
  (confParser <**> helper)
  (  fullDesc
  <> progDesc "design-hs: Smart Design System examples & documentation."
  <> header "Smart Design System"
  )
