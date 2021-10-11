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

import Smart.Html.Application (js, myHead, svgIconClose)


--------------------------------------------------------------------------------
index :: Html
index = document "Smart design system" $
  article
    "Example pages"
    Nothing
    content
    (return ())
  where
  content = do
    H.p $ do
      "These example pages must match the ones presented on the "
      H.a ! A.href "https://design.smart.coop/development/getting-started.html" $
        "original site"
      "."
    H.ul $ do
      H.li $
        H.a ! A.href "app-empty.html" $ "Application, empty page"
      H.li $
        H.a ! A.href "app-navigation.html" $ "Application, navigation bar"
      H.li $
        H.a ! A.href "app-toolbar.html" $ "Application, navigation bar + toolbar"
      H.li $
        H.a ! A.href "app-form.html" $ "Application form"
      H.li $
        H.a ! A.href "app-form--banner.html" $ "Application form, with banner"
      H.li $
        H.a ! A.href "app-form--wizard.html" $ "Application form, with wizard"
      H.li $
        H.a ! A.href "app-form--side-menu.html" $ "Application form, with left menu"
      H.li $
        H.a ! A.href "web-empty.html" $ "Website, empty page"
      H.li $
        H.a ! A.href "blog-post.html" $ "Blog post"


--------------------------------------------------------------------------------
empty :: Html
empty = document "Smart design system" $
  return ()


--------------------------------------------------------------------------------
-- https://design.smart.coop/blog/2021/10/08/smart-announces-an-open-design-system.html
page :: Html
page = document "Smart design system" $
  article
    "Smart announces an open design system"
    (Just "October 8, 2021")
    post
    avatars

post = do
  H.p $ do
    "We're happy to announce we, at Smart Belgium, have started to work on a design system for the web applications of the Smart group. For the first time, but surely not the last, we've decided to make this project open source and available on "
    H.a ! A.href "https://github.com/smartcoop/design" $ "GitHub"
    ". We believe this matches the collaborative values of Smart and will enable various parties of the Smart ecosystem to benefit from it."
  H.p $ do
    "The home of the design system lives at the homepage of this very website. In addition to the blog that you're reading right now, you will be able to learn what's new, find the necessary documentation to get started with "
    H.a ! A.href "https://design.smart.coop/design/how-it-works.html" $ "designing"
    ", using and "
    H.a ! A.href "https://design.smart.coop/development/getting-started.html" $ "developing"
    " the design system yourself, as well as find the right links to be able to contribute."
  H.p "With this design system we are pursuing three goals:"
  H.ul $ do
    H.li "The first goal is to create the visual basis of our future software developments. This may mean the different colors that can be used, the shape and size of a button, or the location of a menu or a form on the screen. This visual work follows and stands on top of the graphic charter introduced in early 2019."
    H.li "The second goal is to create and assemble those design elements in a tool called Figma. Figma is specialized to design software interfaces. In addition of design work by designers, it can also be used by software development teams to prototype what new screens could look like and how they are organized."
    H.li "The third goal is to implement and present the design using web technologies: HTML and CSS. This implementation is meant to be a source of truth for other developers, so they can replicate the design in whatever programming language and technical stack they want."
  H.p $ do
    "We're very excited by what we've done so far and will write additional blog posts soon. In the mean time, you can already join the conversation using "
    H.a ! A.href "https://github.com/smartcoop/design/issues/111" $ "the GitHub issue for commenting for this post"
    ". Feel free to raise your own issues to give feedback and comment on the design system, or start your first contribution."

avatars =
  H.div ! A.class_ "o-container-vertical o-container-vertical--padding-mini" $
    H.ul ! A.class_ "c-avatar-and-text-list" $ do
      H.li ! A.class_ "c-avatar-and-text" $ do
        H.a ! A.class_ "c-avatar c-avatar--img c-avatar--regular" ! A.href "https://github.com/thusc/" $
          H.img ! A.src "https://avatars.githubusercontent.com/u/45588452?v=4" ! A.alt "avatar-image"
        H.div ! A.class_ "c-avatar-and-text__text" $
          H.p "Thu"
      H.li ! A.class_ "c-avatar-and-text" $ do
        H.a ! A.class_ "c-avatar c-avatar--img c-avatar--regular" ! A.href "https://github.com/Wolfr/" $
          H.img ! A.src "https://avatars.githubusercontent.com/u/12690?v=4" ! A.alt "avatar-image"
        H.div ! A.class_ "c-avatar-and-text__text" $
          H.p "Wolfr"


--------------------------------------------------------------------------------
document title body = do
  H.docType
  H.html
    ! A.class_ "u-maximize-height"
    ! A.dir "ltr"
    ! A.lang "en" $ do
    myHead title
    myBody body

article title mdate content authors =
  H.article ! A.class_ "c-blog-article" $ do
    H.div ! A.class_ "c-blog-article__header" $ do
      H.h1 ! A.class_ "c-d-h1" $ title
      maybe (return ()) H.p mdate
    H.div ! A.class_ "c-blog-article__content" $ do
      H.div ! A.class_ "c-display" $
        content
      authors


--------------------------------------------------------------------------------
myBody body =
  H.body ! A.class_ "u-maximize-height" $ do
    myHeader
    H.main ! A.class_ "o-container" $
      H.div ! A.class_ "o-container o-container--medium" $
        H.div ! A.class_ "o-container-vertical" $
          body
    myFooter
    js

myHeader =
  H.header ! A.id "header" $
    H.div ! A.class_ "o-container" $
      H.div ! A.class_ "c-navbar c-navbar--bordered-bottom c-navbar--main" $
        H.div ! A.class_ "c-toolbar" $ do
          H.div ! A.class_ "c-toolbar__left" $
            H.div ! A.class_ "c-toolbar__item" $
              H.a ! A.href "/" $
                H.img ! A.class_ "c-design-system-logo" ! A.src "https://design.smart.coop/images/logo.svg" ! A.alt "Smart"
          H.div ! A.class_ "c-toolbar__right" $
            H.div ! A.class_ "c-toolbar__item" $
              H.nav ! A.class_ "c-design-system-nav" $ do
                H.button ! A.class_ "c-button c-button--borderless c-button--icon c-design-system-nav-open" ! A.type_ "button" ! A.id "c-design-system-nav-open" $
                  H.span ! A.class_ "c-button__content" $ do
                    H.div ! A.class_ "o-svg-icon o-svg-icon-menu  " $
                      svgIconMenu
                    H.div ! A.class_ "u-sr-accessible" $ "Open menu"
                H.button ! A.class_ "c-button c-button--borderless c-button--icon c-design-system-nav-close" ! A.type_ "button" ! A.id "c-design-system-nav-close" $
                  H.span ! A.class_ "c-button__content" $ do
                    H.div ! A.class_ "o-svg-icon o-svg-icon-close  " $
                      svgIconClose
                    H.div ! A.class_ "u-sr-accessible" $ "Close menu"
                H.div ! A.class_ "c-design-system-nav__mobile" $
                  H.ul $ do
                    H.li $ do
                      H.span "Design"
                      H.ul $ do
                        H.li $
                          H.a ! A.href "/design/how-it-works.html" $ "Design workflow"
                        H.li $
                          H.a ! A.href "/design/in-practice/copywriting.html" $ "Design system in practice"
                    H.li $ do
                      H.span "Development"
                      H.ul $
                        H.li $
                          H.a ! A.href "/development/getting-started.html" $ "Getting started"
                    H.li $
                      H.a ! A.href "/blog/index.html" $ "Blog"
                    H.li $
                      H.a ! A.href "/changelog.html" $ "Changelog"
                H.div ! A.class_ "c-design-system-nav__desktop" $
                  H.ul ! A.class_ "c-pill-navigation" $ do
                    H.li ! A.class_ "c-pill-navigation__item c-pill-navigation__item--has-child-menu" $ do
                      H.a ! A.href "#" ! customAttribute "data-menu" "menu1" ! customAttribute "data-menu-samewidth" "true" $ "Design"
                      H.ul ! A.class_ "c-menu c-menu--large" ! A.id "menu1" $ do
                        H.li ! A.class_ "c-menu__item" $
                          H.a ! A.class_ "c-menu__label" ! A.href "/design/how-it-works.html" $ "Design workflow"
                        H.li ! A.class_ "c-menu__item" $
                          H.a ! A.class_ "c-menu__label" ! A.href "/design/in-practice/copywriting.html" $ "Design system in practice"
                    H.li ! A.class_ "c-pill-navigation__item c-pill-navigation__item--has-child-menu" $ do
                      H.a ! A.href "#" ! customAttribute "data-menu" "menu2" ! customAttribute "data-menu-samewidth" "true" $ "Development"
                      H.ul ! A.class_ "c-menu c-menu--large" ! A.id "menu2" $ do
                        H.li ! A.class_ "c-menu__item" $
                          H.a ! A.class_ "c-menu__label" ! A.href "/development/getting-started.html" $ "Getting started"
                        H.li ! A.class_ "c-menu__item" $
                          H.a ! A.class_ "c-menu__label" ! A.href "/development/package-and-repo-links.html" $ "Package and repo links"
                        H.li ! A.class_ "c-menu__divider" ! A.role "presentational" $ ""
                        H.li ! A.class_ "c-menu__item" $
                          H.a ! A.class_ "c-menu__label" ! A.href "/development/component-documentation.html" $ "Component documentation"
                        H.li ! A.class_ "c-menu__divider" ! A.role "presentational" $ ""
                        H.li ! A.class_ "c-menu__item" $
                          H.a ! A.class_ "c-menu__label" ! A.href "/development/browser-support.html" $ "Browser support"
                        H.li ! A.class_ "c-menu__divider" ! A.role "presentational" $ ""
                        H.li ! A.class_ "c-menu__item" $
                          H.a ! A.class_ "c-menu__label" ! A.href "/development/writing-css/architecture.html" $ "CSS architecture"
                        H.li ! A.class_ "c-menu__item" $
                          H.a ! A.class_ "c-menu__label" ! A.href "/development/writing-css/component-structure.html" $ "CSS component structure"
                    H.li ! A.class_ "c-pill-navigation__item" $
                      H.a ! A.href "/blog/index.html" $ "Blog"
                    H.li ! A.class_ "c-pill-navigation__item" $
                      H.a ! A.href "/changelog.html" $ "Changelog"

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
svgIconMenu =
  S.svg
    ! SA.width "24"
    ! SA.height "24"
    ! SA.viewbox "0 0 24 24"
    ! SA.fill "none" $
    S.path
      ! SA.d "M4 7C4 6.44772 4.44772 6 5 6H19C19.5523 6 20 6.44772 20 7C20 7.55228 19.5523 8 19 8H5C4.44772 8 4 7.55228 4 7ZM4 12C4 11.4477 4.44772 11 5 11H19C19.5523 11 20 11.4477 20 12C20 12.5523 19.5523 13 19 13H5C4.44772 13 4 12.5523 4 12ZM4 17C4 16.4477 4.44772 16 5 16H19C19.5523 16 20 16.4477 20 17C20 17.5523 19.5523 18 19 18H5C4.44772 18 4 17.5523 4 17Z"
      ! SA.fill "#595959"

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
