{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  , mainWithConf
  ) where

import qualified Conf
import qualified Conf.Parse                    as CP
import qualified Conf.Types                    as CT
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Examples.Accordion             ( accordions )
import           Examples.Alert                 ( alerts )
import qualified Options.Applicative           as A
import qualified Smart.Html.Dsl                as Dsl
import           Smart.Html.Render             as R
import           System.FilePath.Posix          ( (</>) )
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

-- | Parse the configuration from the cli and run.
main :: IO ExitCode
main = A.execParser CP.confParserInfo >>= mainWithConf

mainWithConf :: CT.Conf -> IO ExitCode
mainWithConf cnf@(CT.Conf CT.FilesystemConf {..}) = do
  Conf.scaffoldFilesystem cnf mempty

  let files =
        second R.renderCanvasWithHeadText
          <$> [ (indexF    , indexHtml)
              , (accordionF, accordionHtml)
              , (alertF    , alertHtml)
              ]

  mapM_ (uncurry T.writeFile) files
  putStrLn @Text "Wrote:"
  confirmWritten $ fst <$> files
  putStrLn @Text "Done!"
  exitSuccess
 where
  examplesF f = _fcOutputDir </> _fcExamplesSubdir </> f
  indexF        = _fcOutputDir </> "index.html"

  accordionF    = examplesF "accordions.html"
  accordionHtml = Dsl.foldCanvas accordions

  alertF        = examplesF "alerts.html"
  alertHtml     = Dsl.foldCanvas alerts

  mkLink (name, file) =
    let href = H.textValue . T.pack $ "./" </> _fcExamplesSubdir </> file
    in  H.a name ! A.href href

  indexHtml = Dsl.SingletonCanvas $ do
    H.title "Smart design-hs"
    H.h1 "Welcome to SmartCoop's Haskell design system!"
    H.br
    H.h2 "Components:"
    links
  links = foldl' mappend mempty [ H.br >> link' | link' <- elLinks ]
  elLinks =
    mkLink <$> [("Accordions", "accordions.html"), ("Alerts", "alerts.html")]

  confirmWritten = putStrLn . T.unlines . fmap T.pack
