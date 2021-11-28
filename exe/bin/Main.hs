{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  , mainWithConf
  ) where

import qualified Conf
import qualified Conf.Parse                    as CP
import qualified Conf.Types                    as CT
import qualified Data.Map                      as M
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Examples.Accordion             ( accordions )
import           Examples.Alert                 ( alerts )
import           Examples.AlertStack            ( alertStacks )
import           Examples.BorderedList          ( borderedLists )
import           Examples.Brand                 ( brands )
import           Examples.Button                ( buttonCanvases )
import           Examples.ButtonToolbar         ( buttonToolbars )
import           Examples.Card                  ( cards )
import           Examples.FileUpload            ( fileUploadResults
                                                , fileUploads
                                                )
import           Examples.Form                  ( formGroups )
import           Examples.GlobalBanner          ( globalBanners )
import           Examples.IconList              ( iconLists )
import           Examples.Loader                ( loaders )
import           Examples.Panel                 ( panels )
import           Examples.Radio                 ( radioGroups )
import           Examples.Ruler                 ( rulers )
import           Examples.Slate                 ( slates )
import           Examples.StatusPill            ( statusPills )
import qualified Options.Applicative           as A
                                         hiding ( style )
import qualified Smart.Html.Dsl                as Dsl
import           Smart.Html.Render             as R
import qualified Smart.Html.Shared.Types       as Types
import           System.FilePath.Posix          ( (</>) )
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

-- | All rendered files can be represented as a Map of the filename, the title of the file (used in linking the file, header of the file etc.)
-- and the canvas it represents.
rendered :: Map FilePath (Types.Title, Dsl.HtmlCanvas)
rendered = M.fromList
  [ ("accordions.html"     , ("Accordions", sampleContents accordions))
  , ("alert-stacks.html"   , ("Alert Stacks", sampleContents alertStacks))
  , ("alerts.html"         , ("Alerts", sampleContents alerts))
  , ("bordered-lists.html" , ("Bordered Lists", sampleContents borderedLists))
  , ("brands.html"         , ("Brands", sampleContents brands))
  , ("button-toolbars.html", ("Button Toolbars", sampleContents buttonToolbars))
  , ("buttons.html"        , ("Buttons", sampleContents buttonCanvases))
  , ("cards.html"          , ("Cards", sampleContents cards))
  , ("file-uploads.html"   , ("File uploads", fileUploadsC))
  , ("forms.html"          , ("Form Groups", sampleContents formGroups))
  , ("global-banners.html" , ("Global Banners", sampleContents globalBanners))
  , ("icon-lists.html"     , ("Icon Lists", sampleContents iconLists))
  , ("loaders.html"        , ("Loaders", sampleContents loaders))
  , ("panels.html"         , ("Panels", sampleContents panels))
  , ("radio-groups.html"   , ("Radio Groups", sampleContents radioGroups))
  , ("rulers.html"         , ("Rulers", rulersC))
  , ("slates.html"         , ("Slates", sampleContents slates))
  , ("status-pills.html"   , ("Status Pills", sampleContents statusPills))
  ]
 where
  rulersC = Dsl.SingletonCanvas @H.ToMarkup (H.h1 "Horizontal ruler")
    Dsl.::~ sampleContents rulers
  fileUploadsC =
    Dsl.SingletonCanvas @H.ToMarkup (H.h1 "Pending file uploads")
      Dsl.::~ sampleContents fileUploads
      Dsl.::~ Dsl.SingletonCanvas @H.ToMarkup (H.h1 "Done file uploads")
      Dsl.::~ sampleContents fileUploadResults

-- | Parse the configuration from the cli and run.
main :: IO ExitCode
main = A.execParser CP.confParserInfo >>= mainWithConf

mainWithConf :: CT.Conf -> IO ExitCode
mainWithConf cnf@(CT.Conf CT.FilesystemConf {..}) = do
  Conf.scaffoldFilesystem cnf mempty

  let indexFile = (indexF, indexHtml)
      files =
        second R.renderCanvasWithHeadText
          <$> indexFile
          :   [ (examplesF fileName, canvas)
              | (fileName, (_, canvas)) <- M.toList rendered
              ]

  mapM_ (uncurry T.writeFile) files
  putStrLn @Text "Wrote:"
  confirmWritten $ fst <$> files
  putStrLn @Text "Done!"
  exitSuccess
 where
  examplesF f = _fcOutputDir </> _fcExamplesSubdir </> f
  indexF = _fcOutputDir </> "index.html"

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
      <$> [ (H.toMarkup title, fileName)
          | (fileName, (title, _)) <- M.toList rendered
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
