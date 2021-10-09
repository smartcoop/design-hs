{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (find)
import qualified Data.Text.Lazy.IO as T
import Text.Blaze (customAttribute)
import qualified Text.Blaze.Html.Renderer.Pretty as Pretty (renderHtml)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 (preEscapedToHtml, toHtml, (!), Html)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import System.Environment (getArgs)

import qualified Text.Blaze.Svg as S (toSvg)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as SA


--------------------------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--pretty"] -> putStr (Pretty.renderHtml page)
    _ -> T.putStr (renderHtml page)


--------------------------------------------------------------------------------
page :: Html
page = document "Smart design system" $
  navbar


--------------------------------------------------------------------------------
navbar = do
  H.div ! A.class_ "c-navbar c-navbar--fixed c-navbar--bordered-bottom" $ do
    H.div ! A.class_ "c-toolbar c-toolbar--medium" $ do
      H.div ! A.class_ "c-toolbar__left" $ do
        H.div ! A.class_ "c-toolbar__item" $ do
          H.a ! A.href "/" $ do
            H.img ! A.src "https://design.smart.coop/images/logo.svg" ! A.alt "Smart Logo"
        H.div ! A.class_ "c-toolbar__item" $ do
          H.nav $ do
            H.ul ! A.class_ "c-pill-navigation" $ do
              H.li ! A.class_ "c-pill-navigation__item" $ do
                H.a ! A.href "#" $ "Activities"
              H.li ! A.class_ "c-pill-navigation__item c-pill-navigation__item--has-child-menu" $ do
                H.a ! A.href "#"
                    ! customAttribute "data-menu" "subMenu-1"
                    ! customAttribute "data-menu-samewidth" "true"
                    $ "Management"
                H.ul ! A.class_ "c-menu c-menu--large" ! A.id "subMenu-1" $ do
                  H.li ! A.class_ "c-menu__item" $ do
                    H.a ! A.class_ "c-menu__label" ! A.href "#" $ "Nav item"
                  H.li ! A.class_ "c-menu__item" $ do
                    H.a ! A.class_ "c-menu__label" ! A.href "#" $ "Nav item"
                  H.li ! A.class_ "c-menu__item" $ do
                    H.a ! A.class_ "c-menu__label" ! A.href "#" $ "Nav item"
              H.li ! A.class_ "c-pill-navigation__item c-pill-navigation__item--has-child-menu" $ do
                H.a ! A.href "#"
                    ! customAttribute "data-menu" "subMenu-2"
                    ! customAttribute "data-menu-samewidth" "true"
                    $ "Documents"
                H.ul ! A.class_ "c-menu c-menu--large" ! A.id "subMenu-2" $ do
                  H.li ! A.class_ "c-menu__item" $ do
                    H.a ! A.class_ "c-menu__label" ! A.href "#" $ "Nav item"
                  H.li ! A.class_ "c-menu__item" $ do
                    H.a ! A.class_ "c-menu__label" ! A.href "#" $ "Nav item"
              H.li ! A.class_ "c-pill-navigation__item" $ do
                H.a ! A.href "#" $ "Members"
              H.li ! A.class_ "c-pill-navigation__item" $ do
                H.a ! A.href "#" $ "Archive"
      H.div ! A.class_ "c-toolbar__right" $ do
        H.div ! A.class_ "c-toolbar__item" $ do
          H.nav $ do
            H.ul ! A.class_ "c-pill-navigation" $ do
              H.li ! A.class_ "c-pill-navigation__item c-pill-navigation__item--has-child-menu" $ do
                H.a ! A.href "#" ! customAttribute "data-menu" "helpMenu" $ do
                  H.div ! A.class_ "o-svg-icon o-svg-icon-circle-help  " $ do
                    svgIconCircleHelp
                  H.span ! A.class_ "u-sr-accessible" $ "Help"
                H.ul ! A.class_ "c-menu c-menu--large" ! A.id "helpMenu" $ do
                  H.li ! A.class_ "c-menu__item" $ do
                    H.a ! A.class_ "c-menu__label" ! A.href "#" $ "About this page"
                  H.li ! A.class_ "c-menu__divider" ! A.role "presentational" $ ""
                  H.li ! A.class_ "c-menu__item" $ do
                    H.a ! A.class_ "c-menu__label" ! A.href "#" $ do
                      H.span "Documentation"
                      H.div ! A.class_ "o-svg-icon o-svg-icon-external-link  " $ do
                        svgIconExternalLink
                  H.li ! A.class_ "c-menu__item" $ do
                    H.a ! A.class_ "c-menu__label" ! A.href "#" $ "Report a bug"
        H.div ! A.class_ "c-toolbar__item" $ do
          H.div ! A.class_ "c-input-group" $ do
            H.div ! A.class_ "c-input-group__prepend" $ do
              H.div ! A.class_ "o-svg-icon o-svg-icon-search  " $ do
                svgIconSearch
            H.input ! A.class_ "c-input" ! A.type_ "text" ! A.placeholder "Search ..."
        H.div ! A.class_ "c-toolbar__item" $ do
          H.a ! A.class_ "c-user-navigation" ! A.href "#" ! customAttribute "data-menu" "userMenu" $ do
            H.div ! A.class_ "c-avatar c-avatar--img c-avatar--regular" $ do
              H.img ! A.src "https://design.smart.coop/images/avatars/1.jpg" ! A.alt "avatar"
          H.ul ! A.class_ "c-menu c-menu--large" ! A.id "userMenu" $ do
            H.li ! A.class_ "c-menu__item" $ do
              H.a ! A.class_ "c-menu__label" ! A.href "#" $ "My profile"
            H.li ! A.class_ "c-menu__divider" ! A.role "presentational" $ ""
            H.li ! A.class_ "c-menu__item" $ do
              H.a ! A.class_ "c-menu__label" ! A.href "#" $ "Sign out"


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
myHead title =
  H.head $ do
    H.meta ! A.charset "utf-8"
    H.title title
    H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
    H.meta ! A.name "robots" ! A.content "noindex"
    H.link
      ! A.rel "stylesheet"
      ! A.href "https://design.smart.coop/css/main.css"

myBody body =
  H.body ! A.class_ "u-maximize-height u-overflow-hidden" $ do
    H.div ! A.class_ "c-app-layout" $
      body
    js

js = do
  H.script ! A.src "https://design.smart.coop/js/bundle-prototype.js" $ ""
  H.script ! A.src "https://design.smart.coop/js/bundle-client.js" $ ""


--------------------------------------------------------------------------------
svgIconCircleHelp :: Html
svgIconCircleHelp =
  S.svg
    ! SA.width "24"
    ! SA.height "24"
    ! SA.viewbox "0 0 24 24"
    ! SA.fill "none" $ do
    S.path
      ! SA.d "M12 4C9.87827 4 7.84344 4.84285 6.34315 6.34315C4.84285 7.84344 4 9.87827 4 12C4 14.1217 4.84285 16.1566 6.34315 17.6569C7.84344 19.1571 9.87827 20 12 20C14.1217 20 16.1566 19.1571 17.6569 17.6569C19.1571 16.1566 20 14.1217 20 12C20 9.87827 19.1571 7.84344 17.6569 6.34315C16.1566 4.84285 14.1217 4 12 4ZM2 12C2 6.477 6.477 2 12 2C17.523 2 22 6.477 22 12C22 17.523 17.523 22 12 22C6.477 22 2 17.523 2 12Z"
      ! SA.fill "#595959"
    S.path
      ! SA.d "M12 14C11.7348 14 11.4804 13.8946 11.2929 13.7071C11.1054 13.5196 11 13.2652 11 13V12C11 11.7348 11.1054 11.4804 11.2929 11.2929C11.4804 11.1054 11.7348 11 12 11C12.2652 11 12.5196 11.1054 12.7071 11.2929C12.8946 11.4804 13 11.7348 13 12V13C13 13.2652 12.8946 13.5196 12.7071 13.7071C12.5196 13.8946 12.2652 14 12 14ZM10.5 16.5C10.5 16.1022 10.658 15.7206 10.9393 15.4393C11.2206 15.158 11.6022 15 12 15C12.3978 15 12.7794 15.158 13.0607 15.4393C13.342 15.7206 13.5 16.1022 13.5 16.5C13.5 16.8978 13.342 17.2794 13.0607 17.5607C12.7794 17.842 12.3978 18 12 18C11.6022 18 11.2206 17.842 10.9393 17.5607C10.658 17.2794 10.5 16.8978 10.5 16.5Z"
      ! SA.fill "#595959"
    S.path
      ! SA.d "M12.3899 7.811C11.4329 7.766 10.6299 8.301 10.4859 9.164C10.4356 9.41907 10.2878 9.6445 10.0741 9.79249C9.86029 9.94047 9.59729 9.99938 9.34081 9.95672C9.08434 9.91406 8.85456 9.77319 8.7002 9.56397C8.54584 9.35476 8.47903 9.09364 8.51394 8.836C8.86994 6.7 10.8169 5.734 12.4849 5.814C13.3389 5.854 14.2179 6.161 14.8939 6.793C15.5869 7.44 15.9999 8.368 15.9999 9.5C15.9999 10.791 15.4919 11.749 14.6169 12.332C13.8139 12.867 12.8289 13 11.9999 13C11.7347 13 11.4804 12.8946 11.2928 12.7071C11.1053 12.5196 10.9999 12.2652 10.9999 12C10.9999 11.7348 11.1053 11.4804 11.2928 11.2929C11.4804 11.1054 11.7347 11 11.9999 11C12.6699 11 13.1859 10.883 13.5079 10.668C13.7579 10.501 13.9999 10.208 13.9999 9.5C13.9999 8.882 13.7879 8.497 13.5279 8.254C13.2509 7.995 12.8479 7.834 12.3899 7.811Z"
      ! SA.fill "#595959"

svgIconExternalLink :: Html
svgIconExternalLink =
  S.svg
    ! SA.width "24"
    ! SA.height "24"
    ! SA.viewbox "0 0 24 24"
    ! SA.fill "none" $
    S.path
      ! SA.d "M14 5C13.7348 5 13.4804 4.89464 13.2929 4.70711C13.1054 4.51957 13 4.26522 13 4C13 3.73478 13.1054 3.48043 13.2929 3.29289C13.4804 3.10536 13.7348 3 14 3H20C20.2652 3 20.5196 3.10536 20.7071 3.29289C20.8946 3.48043 21 3.73478 21 4V10C21 10.2652 20.8946 10.5196 20.7071 10.7071C20.5196 10.8946 20.2652 11 20 11C19.7348 11 19.4804 10.8946 19.2929 10.7071C19.1054 10.5196 19 10.2652 19 10V6.414L9.707 15.707C9.5184 15.8892 9.2658 15.99 9.0036 15.9877C8.7414 15.9854 8.49059 15.8802 8.30518 15.6948C8.11977 15.5094 8.0146 15.2586 8.01233 14.9964C8.01005 14.7342 8.11084 14.4816 8.293 14.293L17.586 5H14ZM3 7C3 6.46957 3.21071 5.96086 3.58579 5.58579C3.96086 5.21071 4.46957 5 5 5H10C10.2652 5 10.5196 5.10536 10.7071 5.29289C10.8946 5.48043 11 5.73478 11 6C11 6.26522 10.8946 6.51957 10.7071 6.70711C10.5196 6.89464 10.2652 7 10 7H5V19H17V14C17 13.7348 17.1054 13.4804 17.2929 13.2929C17.4804 13.1054 17.7348 13 18 13C18.2652 13 18.5196 13.1054 18.7071 13.2929C18.8946 13.4804 19 13.7348 19 14V19C19 19.5304 18.7893 20.0391 18.4142 20.4142C18.0391 20.7893 17.5304 21 17 21H5C4.46957 21 3.96086 20.7893 3.58579 20.4142C3.21071 20.0391 3 19.5304 3 19V7Z"
      ! SA.fill "#595959"

svgIconSearch :: Html
svgIconSearch =
  S.svg
    ! SA.width "24"
    ! SA.height "24"
    ! SA.viewbox "0 0 24 24"
    ! SA.fill "none" $
    S.path
      ! SA.d "M10 4C6.68629 4 4 6.68629 4 10C4 13.3137 6.68629 16 10 16C13.3137 16 16 13.3137 16 10C16 6.68629 13.3137 4 10 4ZM2 10C2 5.58172 5.58172 2 10 2C14.4183 2 18 5.58172 18 10C18 11.8487 17.3729 13.551 16.3199 14.9056L21.7071 20.2929C22.0976 20.6834 22.0976 21.3166 21.7071 21.7071C21.3166 22.0976 20.6834 22.0976 20.2929 21.7071L14.9056 16.3199C13.551 17.3729 11.8487 18 10 18C5.58172 18 2 14.4183 2 10Z"
      ! SA.fill "#595959"
