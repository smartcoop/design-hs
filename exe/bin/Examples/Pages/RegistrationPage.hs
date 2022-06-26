module Examples.Pages.RegistrationPage
  ( registrationPage
  ) where

import           Smart.Html.Dsl                 ( HtmlCanvas )
import qualified Smart.Html.Dsl                as Dsl
import qualified Smart.Html.Misc               as Misc
import           Smart.Html.Navbar
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!), Html )
import qualified Text.Blaze.Html5.Attributes   as A


registrationPage :: HtmlCanvas
registrationPage = Dsl.SingletonCanvas $ do
  H.toMarkup Misc.registration
