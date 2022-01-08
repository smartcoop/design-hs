{-# LANGUAGE OverloadedStrings #-}

module Smart.Html.Navbar where

import           Smart.Html.Avatar
import           Smart.Html.Brand
import           Smart.Html.Shared.Html.Icons
import Text.Blaze (customAttribute)
import Text.Blaze.Html5 ((!), Html)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data Navbar = Navbar
  deriving Show

instance H.ToMarkup Navbar where
  toMarkup = mkNavbar

mkNavbar _ = navbar exampleTree

exampleTree :: [(Text, Text, [(Text, Text)])]
exampleTree =
  [ ("Activities", "#", [])
  , ("Management", "#",
      [ ("Nav item", "#")
      , ("Nav item", "#")
      , ("Nav item", "#")
      ])
  , ("Documents", "#",
      [ ("Nav item", "#")
      , ("Nav item", "#")
      ])
  , ("Members", "#", [])
  , ("Archive", "#", [])
  ]

toNavbar tree =
  mapM_ toplevel (zip tree [1..])
  where
  toplevel :: ((Text, Text, [(Text, Text)]), Int) -> Html
  toplevel ((a, lnk, []), _) =
    H.li ! A.class_ "c-pill-navigation__item" $
      H.a ! A.href (H.toValue lnk) $ H.toHtml a
  toplevel ((a, _, bs), n) =
    H.li ! A.class_ "c-pill-navigation__item c-pill-navigation__item--has-child-menu" $ do
      H.button ! A.type_ "button"
               ! customAttribute "data-menu" (H.toValue $ "subMenu-" ++ show n)
               ! customAttribute "data-menu-samewidth" "true"
               $ H.toHtml a
      H.ul
        ! A.class_ "c-menu c-menu--large"
        ! A.id (H.toValue $ "subMenu-" ++ show n) $
        mapM_ sublevel bs
  sublevel (b, lnk) =
    H.li ! A.class_ "c-menu__item" $ do
      H.a ! A.class_ "c-menu__label" ! A.href (H.toValue lnk) $ H.toHtml b

navbar tree = do
  H.header $
    H.div ! A.class_ "c-navbar c-navbar--fixed c-navbar--bordered-bottom" $ do
      H.div ! A.class_ "c-toolbar" $ do
        H.div ! A.class_ "c-toolbar__left" $ do
          H.div ! A.class_ "c-toolbar__item" $ do
            H.div ! A.class_ "c-brand c-brand--xsmall" $ do
              H.a ! A.href "/" $ do
                H.img ! A.src "https://design.smart.coop/images/logo.svg" ! A.alt "Smart"
          H.div ! A.class_ "c-toolbar__item" $ do
            H.nav $ do
              H.ul ! A.class_ "c-pill-navigation" $
                toNavbar tree
        H.div ! A.class_ "c-toolbar__right" $ do
          H.div ! A.class_ "c-toolbar__item" $ do
            H.nav $ do
              H.ul ! A.class_ "c-pill-navigation" $ do
                H.li ! A.class_ "c-pill-navigation__item c-pill-navigation__item--has-child-menu" $ do
                  H.button ! A.type_ "button" ! customAttribute "data-menu" "helpMenu" $ do
                    H.div ! A.class_ "o-svg-icon o-svg-icon-circle-help  " $ do
                      H.toMarkup svgIconCircleHelp
                    H.span ! A.class_ "u-sr-accessible" $ "Help"
                  H.ul ! A.class_ "c-menu c-menu--large" ! A.id "helpMenu" $ do
                    H.li ! A.class_ "c-menu__item" $ do
                      H.a ! A.class_ "c-menu__label" ! A.href "#" $ "About this page"
                    H.li ! A.class_ "c-menu__divider" ! A.role "presentational" $ ""
                    H.li ! A.class_ "c-menu__item" $ do
                      H.a ! A.class_ "c-menu__label" ! A.href "#" $ do
                        H.span "Documentation"
                        H.div ! A.class_ "o-svg-icon o-svg-icon-external-link  " $ do
                          H.toMarkup svgIconExternalLink
                    H.li ! A.class_ "c-menu__item" $ do
                      H.a ! A.class_ "c-menu__label" ! A.href "#" $ "Report a bug"
          H.div ! A.class_ "c-toolbar__item" $ do
            H.div ! A.class_ "c-input-with-icon" $ do
              H.div ! A.class_ "o-svg-icon o-svg-icon-search  " $ do
                H.toMarkup svgIconSearch
              H.input ! A.class_ "c-input" ! A.type_ "text" ! A.placeholder "Search ..."
          H.div ! A.class_ "c-toolbar__item" $ do
            H.a ! A.class_ "c-user-navigation" ! A.href "#" ! customAttribute "data-menu" "userMenu" $ do
              H.toMarkup $
                Avatar (AvatarImage "https://design.smart.coop/images/avatars/1.jpg")
                  Regular AvNoAdditionalContent
            H.ul ! A.class_ "c-menu c-menu--large" ! A.id "userMenu" $ do
              H.li ! A.class_ "c-menu__item" $ do
                H.a ! A.class_ "c-menu__label" ! A.href "#" $ "My profile"
              H.li ! A.class_ "c-menu__divider" ! A.role "presentational" $ ""
              H.li ! A.class_ "c-menu__item" $ do
                H.a ! A.class_ "c-menu__label" ! A.href "#" $ "Sign out"
