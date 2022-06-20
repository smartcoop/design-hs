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
import           Examples.AlertDialog           ( alertDialogs )
import           Examples.AlertStack            ( alertStacks )
import           Examples.BorderedList          ( borderedLists )
import           Examples.Brand                 ( brands )
import           Examples.Button                ( buttonCanvases )
import           Examples.ButtonToolbar         ( buttonToolbars )
import           Examples.Card                  ( cards )
import           Examples.FileAttachment        ( fileAttachments )
import           Examples.FileUpload            ( fileUploadResults
                                                , fileUploads
                                                )
import           Examples.Form                  ( formGroups )
import           Examples.GlobalBanner          ( globalBanners )
import           Examples.IconList              ( iconLists )
import           Examples.KeyValue              ( keyValueGroups )
import           Examples.Loader                ( loaders )
import           Examples.Navbar                ( navbars )
import           Examples.Pages.EmptyPage       ( emptyPage )
import           Examples.Pages.Errors          ( notFound
                                                , notFoundWebsite
                                                )
import           Examples.Pages.LandingPage     ( landingPage
                                                , navigation
                                                )
import           Examples.Pages.MainHeader      ( mainHeader
                                                , mainHeaderWebsite
                                                )
import           Examples.Panel                 ( panels )
import           Examples.Radio                 ( radioGroups )
import           Examples.Ruler                 ( rulers )
import           Examples.SideMenu              ( sideMenus )
import           Examples.Slate                 ( slates )
import           Examples.StatusPill            ( statusPills )
import qualified Options.Applicative           as A
                                         hiding ( style )
import qualified Smart.Html.Dsl                as Dsl
import           Smart.Html.Render             as R
import qualified Smart.Html.Shared.Html.Helpers
                                               as Helpers
import qualified Smart.Html.Shared.Types       as Types
import           System.FilePath.Posix          ( (</>) )
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

-- | Parse the configuration from the cli and run.
main :: IO ExitCode
main = A.execParser CP.confParserInfo >>= mainWithConf

mainWithConf :: CT.Conf -> IO ExitCode
mainWithConf cnf@(CT.Conf CT.FilesystemConf {..}) = do
  Conf.scaffoldFilesystem cnf ["components", "pages"]

  let indexFile      = (indexF, indexHtml)
      componentsFile = (componentsF, componentsHtml)
      pagesFile      = (pagesF, pagesHtml)
      files =
        second R.renderCanvasText
          <$> indexFile
          :   componentsFile
          :   pagesFile
          :   [ (_fcOutputDir </> fileName, canvas)
              | (fileName, (_, canvas)) <- M.toList components <> pages
              ]

  mapM_ (uncurry T.writeFile) files
  putStrLn @Text "Wrote:"
  confirmWritten $ fst <$> files
  putStrLn @Text "Done!"
  exitSuccess
 where
  indexF         = _fcOutputDir </> "index.html"
  componentsF    = _fcOutputDir </> "components" </> "index.html"
  pagesF         = _fcOutputDir </> "pages" </> "index.html"

  indexHtml      = landingPage

  componentsHtml = Dsl.SingletonCanvas $ do
    H.toMarkup navigation
    mainDisplay $ do
      H.h1 "Components"
      componentLinks

  pagesHtml = Dsl.SingletonCanvas $ do
    H.toMarkup navigation
    mainDisplay $ do
      H.h1 "Pages"
      pageLinks

  mainDisplay content =
    H.main
      $ H.div
      ! A.class_ "o-container o-container--medium"
      $ H.div
      ! A.class_ "o-container-vertical"
      $ H.div
      ! A.class_ "c-display"
      $ content

  -- TODO Remove duplication.
  componentLinks = Helpers.unorderedList componentLinks'
  componentLinks' =
    mkLink
      <$> [ (H.toMarkup title, fileName)
          | (fileName, (title, _)) <- M.toList components
          ]
  pageLinks = Helpers.unorderedList pageLinks'
  pageLinks' =
    mkLink
      <$> [ (H.toMarkup title, fileName) | (fileName, (title, _)) <- pages ]

  mkLink (name, file) =
    let fileURI = "/" </> file in H.a name ! A.href (H.stringValue fileURI)

  confirmWritten = putStrLn . T.unlines . fmap T.pack

-- | All rendered files can be represented as a Map of the filename, the title of the file (used in linking the file, header of the file etc.)
-- and the canvas it represents.
components :: Map FilePath (Types.Title, Dsl.HtmlCanvas)
components =
  M.fromList
    $   first ("components" </>)
    <$> [ ("accordions.html"   , ("Accordions", sampleContents accordions))
        , ("alerts.html"       , ("Alerts", sampleContents alerts))
        , ("alert-dialogs.html", ("Alert dialogs", sampleContents alertDialogs))
        , ("alert-stacks.html" , ("Alert stacks", sampleContents alertStacks))
        , ( "bordered-lists.html"
          , ("Bordered lists", sampleContents borderedLists)
          )
        , ("brands.html", ("Brands", sampleContents brands))
        , ( "button-toolbars.html"
          , ("Button toolbars", sampleContents buttonToolbars)
          )
        , ("buttons.html", ("Buttons", sampleContents buttonCanvases))
        , ("cards.html"  , ("Cards", sampleContents cards))
        , ( "file-attachments.html"
          , ("File attachments", sampleContents fileAttachments)
          )
        , ("file-uploads.html", ("File uploads", fileUploadsC))
        , ("forms.html"       , ("Form groups", sampleContents formGroups))
        , ( "global-banners.html"
          , ("Global banners", sampleContents globalBanners)
          )
        , ("icon-lists.html"  , ("Icon lists", sampleContents iconLists))
        , ("key-values.html"  , ("Key values", sampleContents keyValueGroups))
        , ("loaders.html"     , ("Loaders", sampleContents loaders))
        , ("navbars.html"     , ("Navbars", sampleContents navbars))
        , ("panels.html"      , ("Panels", sampleContents panels))
        , ("radio-groups.html", ("Radio groups", sampleContents radioGroups))
        , ("rulers.html"      , ("Rulers", rulersC))
        , ("slates.html"      , ("Slates", sampleContents slates))
        , ("side-menus.html"  , ("Side menus", sampleContents sideMenus))
        , ("status-pills.html", ("Status pills", sampleContents statusPills))
        ]
 where
  rulersC = Dsl.SingletonCanvas @H.ToMarkup (H.h1 "Horizontal ruler")
    Dsl.::~ sampleContents rulers
  fileUploadsC =
    Dsl.SingletonCanvas @H.ToMarkup (H.h1 "Pending file uploads")
      Dsl.::~ sampleContents fileUploads
      Dsl.::~ Dsl.SingletonCanvas @H.ToMarkup (H.h1 "Done file uploads")
      Dsl.::~ sampleContents fileUploadResults

pages :: [(FilePath, (Types.Title, Dsl.HtmlCanvas))]
pages =
  first ("pages" </>)
    <$> [ ("empty.html", ("Empty page", emptyPage))
        , ( "main-header-website.html"
          , ("Main header (website)", mainHeaderWebsite)
          )
        , ("main-header.html", ("Main header (application)", mainHeader))
        , ("404-website.html", ("404 Not found (website)", notFoundWebsite))
        , ("404.html"        , ("404 Not found (application)", notFound))
        ]

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
