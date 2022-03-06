{-# LANGUAGE OverloadedStrings #-}

module Smart.Html.Navbar where

import           Smart.Html.Avatar
import           Smart.Html.Brand
import           Smart.Html.Shared.Html.Icons
import           Smart.Html.Shared.Types (Link, Title)
import Text.Blaze (customAttribute)
import Text.Blaze.Html5 ((!), Html)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

newtype Navbar = Navbar [Entry]

instance H.ToMarkup Navbar where
  toMarkup = mkNavbar

mkNavbar (Navbar entries) = navbar entries

-- An entry has a name, then either a link or subentries.
data Entry = Entry Title Action

-- This represents either a link or subentries (associated to an entry).
data Action = Link Link | SubEntries [SubEntry]

-- A subentry is just a pair name, link.
data SubEntry = SubEntry Title Link

toNavbar tree =
  mapM_ toplevel (zip tree [1..])
  where
  toplevel :: (Entry, Int) -> Html
  toplevel (Entry a (Link lnk), _) =
    H.li ! A.class_ "c-pill-navigation__item" $
      H.a ! A.href (H.toValue lnk) $ H.toHtml a
  toplevel (Entry a (SubEntries bs), n) =
    H.li ! A.class_ "c-pill-navigation__item c-pill-navigation__item--has-child-menu" $ do
      H.button ! A.type_ "button"
               ! customAttribute "data-menu" (H.toValue $ "subMenu-" ++ show n)
               ! customAttribute "data-menu-samewidth" "true"
               $ H.toHtml a
      H.ul
        ! A.class_ "c-menu c-menu--large"
        ! A.id (H.toValue $ "subMenu-" ++ show n) $
        mapM_ sublevel bs
  sublevel (SubEntry b lnk) =
    H.li ! A.class_ "c-menu__item" $
      H.a ! A.class_ "c-menu__label" ! A.href (H.toValue lnk) $ H.toHtml b

navbar tree =
  H.header $
    H.div ! A.class_ "c-navbar c-navbar--fixed c-navbar--bordered-bottom" $
      H.div ! A.class_ "c-toolbar" $ do
        H.div ! A.class_ "c-toolbar__left" $ do
          H.div ! A.class_ "c-toolbar__item" $
            H.toMarkup $
              BrandXSmall "/" "https://design.smart.coop/images/logo.svg" "Smart"
          H.div ! A.class_ "c-toolbar__item" $
            H.nav $
              H.ul ! A.class_ "c-pill-navigation" $
                toNavbar tree
        H.div ! A.class_ "c-toolbar__right" $ do
          H.div ! A.class_ "c-toolbar__item" $
            H.nav $
              H.ul ! A.class_ "c-pill-navigation" $
                H.li ! A.class_ "c-pill-navigation__item c-pill-navigation__item--has-child-menu" $ do
                  H.button ! A.type_ "button" ! customAttribute "data-menu" "helpMenu" $ do
                    H.div ! A.class_ "o-svg-icon o-svg-icon-circle-help  " $
                      H.toMarkup svgIconCircleHelp
                    H.span ! A.class_ "u-sr-accessible" $ "Help"
                  H.ul ! A.class_ "c-menu c-menu--large" ! A.id "helpMenu" $ do
                    H.li ! A.class_ "c-menu__item" $
                      H.a ! A.class_ "c-menu__label" ! A.href "#" $ "About this page"
                    H.li ! A.class_ "c-menu__divider" ! A.role "presentational" $ ""
                    H.li ! A.class_ "c-menu__item" $
                      H.a ! A.class_ "c-menu__label" ! A.href "#" $ do
                        H.span "Documentation"
                        H.div ! A.class_ "o-svg-icon o-svg-icon-external-link  " $
                          H.toMarkup svgIconExternalLink
                    H.li ! A.class_ "c-menu__item" $
                      H.a ! A.class_ "c-menu__label" ! A.href "#" $ "Report a bug"
          H.div ! A.class_ "c-toolbar__item" $
            H.div ! A.class_ "c-input-with-icon" $ do
              H.div ! A.class_ "o-svg-icon o-svg-icon-search  " $
                H.toMarkup svgIconSearch
              H.input ! A.class_ "c-input" ! A.type_ "text" ! A.placeholder "Search ..."
          H.div ! A.class_ "c-toolbar__item" $ do
            H.a ! A.class_ "c-user-navigation" ! A.href "#" ! customAttribute "data-menu" "userMenu" $
              H.toMarkup $
                Avatar (AvatarImage "https://design.smart.coop/images/avatars/1.jpg")
                  Regular AvNoAdditionalContent
            H.ul ! A.class_ "c-menu c-menu--large" ! A.id "userMenu" $ do
              H.li ! A.class_ "c-menu__item" $
                H.a ! A.class_ "c-menu__label" ! A.href "#" $ "My profile"
              H.li ! A.class_ "c-menu__divider" ! A.role "presentational" $ ""
              H.li ! A.class_ "c-menu__item" $
                H.a ! A.class_ "c-menu__label" ! A.href "#" $ "Sign out"
