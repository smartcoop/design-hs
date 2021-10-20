{-# LANGUAGE ApplicativeDo #-}
module Conf.Parse
  ( confParser
  ) where

import           Conf.Types
import           Options.Applicative

confParser :: Parser Conf
confParser = do
  _cOutputDir <- opDir
  pure Conf { .. }
 where
  opDir = strOption
    (long "output-dir" <> short 'D' <> help helpText <> metavar "DIRECTORY")
  helpText
    = "Output directory under which to construct the tree, existing files will be overwritten."
