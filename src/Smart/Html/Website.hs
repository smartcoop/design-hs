{-# LANGUAGE OverloadedStrings #-}

module Smart.Html.Website where

import Text.Blaze (customAttribute)
import qualified Text.Blaze.Html.Renderer.Pretty as Pretty (renderHtml)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 (preEscapedToHtml, toHtml, (!), Html)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Svg as S (toSvg)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as SA

import Smart.Html.Application (js, myHead)


--------------------------------------------------------------------------------
-- https://design.smart.coop/blog/2021/10/08/smart-announces-an-open-design-system.html
page :: Html
page = document "Smart design system" $ do
  return ()


--------------------------------------------------------------------------------
document title body = do
  H.docType
  H.html
    ! A.class_ "u-maximize-height"
    ! A.dir "ltr"
    ! A.lang "en" $ do
    myHead title
    myBody body


--------------------------------------------------------------------------------
myBody body =
  H.body ! A.class_ "u-maximize-height" $ do
    H.header ! A.id "header" $
      return ()
    H.main ! A.class_ "o-container" $
      H.div ! A.class_ "o-container o-container--medium" $
        H.div ! A.class_ "o-container-vertical" $
          body
    myFooter
    js

myFooter =
  H.footer ! A.id "footer" $
    H.div ! A.class_ "o-container-vertical" $
      H.div ! A.class_ "o-container" $ do
        H.hr ! A.class_ "c-hr"
        H.ul ! A.class_ "c-bordered-list-horizontal c-bordered-list-horizontal--muted" $
          H.li $
            H.a ! A.href "https://github.com/smartcoop/design" $
              H.div ! A.class_ "o-flex" $ do
                H.div ! A.class_ "u-spacer-right-s" $
                  H.div ! A.class_ "o-svg-icon o-svg-icon-github" $
                    svgIconGitHub
                "Share feedback on GitHub"

--------------------------------------------------------------------------------
svgIconGitHub =
  S.svg
    ! SA.width "24"
    ! SA.height "24"
    ! SA.viewbox "0 0 24 24"
    ! SA.fill "none" $
    S.path
      ! SA.fillRule "evenodd"
      ! SA.clipRule "evenodd"
      ! SA.d "M12 2C6.475 2 2 6.475 2 12C2 16.425 4.8625 20.1625 8.8375 21.4875C9.3375 21.575 9.525 21.275 9.525 21.0125C9.525 20.775 9.5125 19.9875 9.5125 19.15C7 19.6125 6.35 18.5375 6.15 17.975C6.0375 17.6875 5.55 16.8 5.125 16.5625C4.775 16.375 4.275 15.9125 5.1125 15.9C5.9 15.8875 6.4625 16.625 6.65 16.925C7.55 18.4375 8.9875 18.0125 9.5625 17.75C9.65 17.1 9.9125 16.6625 10.2 16.4125C7.975 16.1625 5.65 15.3 5.65 11.475C5.65 10.3875 6.0375 9.4875 6.675 8.7875C6.575 8.5375 6.225 7.5125 6.775 6.1375C6.775 6.1375 7.6125 5.875 9.525 7.1625C10.325 6.9375 11.175 6.825 12.025 6.825C12.875 6.825 13.725 6.9375 14.525 7.1625C16.4375 5.8625 17.275 6.1375 17.275 6.1375C17.825 7.5125 17.475 8.5375 17.375 8.7875C18.0125 9.4875 18.4 10.375 18.4 11.475C18.4 15.3125 16.0625 16.1625 13.8375 16.4125C14.2 16.725 14.5125 17.325 14.5125 18.2625C14.5125 19.6 14.5 20.675 14.5 21.0125C14.5 21.275 14.6875 21.5875 15.1875 21.4875C17.1727 20.8173 18.8977 19.5415 20.1198 17.8395C21.3419 16.1376 21.9995 14.0953 22 12C22 6.475 17.525 2 12 2Z"
      ! SA.fill "#595959"
