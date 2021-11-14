{-# LANGUAGE ConstraintKinds #-}
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
import           Examples.AlertStack            ( alertStacks )
import           Examples.BorderedList          ( borderedLists )
import           Examples.Brand                 ( brands )
import           Examples.Button                ( buttonCanvases )
import           Examples.ButtonToolbar         ( buttonToolbars )
import           Examples.GlobalBanner          ( globalBanners )
import           Examples.Radio                 ( radioGroups )
import           Examples.Ruler                 ( rulers )
import           Examples.Slate                 ( slates )
import qualified Options.Applicative           as A
                                         hiding ( style )
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
          <$> [ (indexF         , indexHtml)
              , (accordionF     , accordionHtml)
              , (alertF         , alertHtml)
              , (buttonF        , buttonHtml)
              , (slateF         , slateHtml)
              , (radioGroupF    , radioGroupHtml)
              , (alertStacksF   , alertStacksHtml)
              , (borderedListsF , borderedListsHtml)
              , (brandsF        , brandsHtml)
              , (buttonToolbarsF, buttonToolbarsHtml)
              , (globalBannersF , globalBannersHtml)
              , (rulersF        , rulersHtml)
              ]

  mapM_ (uncurry T.writeFile) files
  putStrLn @Text "Wrote:"
  confirmWritten $ fst <$> files
  putStrLn @Text "Done!"
  exitSuccess
 where
  examplesF f = _fcOutputDir </> _fcExamplesSubdir </> f
  indexF             = _fcOutputDir </> "index.html"

  accordionF         = examplesF "accordions.html"
  accordionHtml      = sampleContents accordions

  alertF             = examplesF "alerts.html"
  alertHtml          = sampleContents alerts

  buttonF            = examplesF "buttons.html"
  buttonHtml         = sampleContents buttonCanvases

  slateF             = examplesF "slates.html"
  slateHtml          = sampleContents slates

  radioGroupF        = examplesF "radio-groups.html"
  radioGroupHtml     = sampleContents radioGroups

  alertStacksF       = examplesF "alert-stacks.html"
  alertStacksHtml    = sampleContents alertStacks

  borderedListsF     = examplesF "bordered-lists.html"
  borderedListsHtml  = sampleContents borderedLists

  brandsF            = examplesF "brands.html"
  brandsHtml         = sampleContents brands

  buttonToolbarsF    = examplesF "button-toolbars.html"
  buttonToolbarsHtml = sampleContents buttonToolbars

  globalBannersF     = examplesF "global-banners.html"
  globalBannersHtml  = sampleContents globalBanners

  rulersF            = examplesF "rulers.html"
  rulersHtml         = Dsl.SingletonCanvas @H.ToMarkup (H.h1 "Horizontal ruler")
    Dsl.::~ sampleContents rulers

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
    mkLink
      <$> [ ("Accordions"     , "accordions.html")
          , ("Alerts"         , "alerts.html")
          , ("Buttons"        , "buttons.html")
          , ("Slates"         , "slates.html")
          , ("Radio groups"   , "radio-groups.html")
          , ("Alert stacks"   , "alert-stacks.html")
          , ("Bordered lists" , "bordered-lists.html")
          , ("Brands"         , "brands.html")
          , ("Button toolbars", "button-toolbars.html")
          , ("Global banners" , "global-banners.html")
          , ("Rulers"         , "rulers.html")
          ]

  confirmWritten = putStrLn . T.unlines . fmap T.pack

sampleContents
  :: forall a f
   . (Foldable f, Functor f, H.ToMarkup a)
  => f a
  -> Dsl.HtmlCanvas
sampleContents elems = Dsl.foldCanvas $ sampleContent <$> elems

sampleContent :: forall a . H.ToMarkup a => a -> Dsl.HtmlCanvas
sampleContent elem' =
  let divContents = Dsl.SingletonCanvas @H.ToMarkup elem'
  in  Dsl.SingletonCanvas . div' $ H.toMarkup divContents
  where div' = H.div ! A.class_ "br-sample-content"
