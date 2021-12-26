module Smart.Html.Wizard
  ( WizardStep(..)
  , WizardIndicator(..)
  ) where

import qualified Smart.Html.Shared.Types       as Types
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

newtype WizardIndicator = WizardIndicator { _unWizardIndicator :: Text }
                        deriving (Eq, Show, IsString) via Text

instance H.ToMarkup WizardIndicator where
  toMarkup = maybeIndicator . Just

maybeIndicator = (H.div ! A.class_ "c-wizard__indicator")
  . maybe mempty (H.toMarkup . _unWizardIndicator)

data WizardStep =
  WizardStepPending WizardIndicator Types.Body (Maybe Types.URI)
  | WizardStepComplete Types.Body (Maybe Types.URI)
  deriving (Eq, Show)

instance H.ToMarkup WizardStep where
  toMarkup = \case
    WizardStepPending ind body mURI -> mkWizardWith (Just ind) body mURI
    WizardStepComplete body mURI    -> mkWizardWith Nothing body mURI
   where
    mkWizardWith mIndicator body mURI =
      H.li
        .  (H.a ! A.class_ "c-wizard__item c-wizard--complete" ! A.href href)
        $  indM
        >> bodyM
     where
      href  = maybe "#" (H.textValue . Types._unURI) mURI
      indM  = maybeIndicator mIndicator
      bodyM = H.div ! A.class_ "c-wizard__label" $ H.toMarkup body

newtype Wizard = Wizard { _wizardSteps :: [ WizardStep ] }
               deriving (Eq, Show)

instance H.ToMarkup Wizard where
  toMarkup (Wizard steps) =
    (H.nav ! A.class_ "c-wizard c-wizard--bordered")
      .   H.ul
      .   mconcat
      $   H.toMarkup
      <$> steps
