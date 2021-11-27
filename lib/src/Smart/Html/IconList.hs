module Smart.Html.IconList
  ( IconListElem(..)
  , IconList(..)
  ) where

import           Smart.Html.Dsl
import           Smart.Html.Shared.Html.Icons   ( IconDiv )
import qualified Smart.Html.Shared.Types       as Types
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

-- | A single element of an icon list 
data IconListElem where
  IconListElem ::KnownSymbol iconType => { _elemIcon :: IconDiv iconType
                                         , _elemBody :: Types.Body
                                         } -> IconListElem

-- | A full-blown icon list
newtype IconList = IconList [IconListElem]

instance H.ToMarkup IconListElem where
  toMarkup (IconListElem icon body) =
    (H.li ! A.class_ "c-icon-list__item")
      .   H.toMarkup
      $   icon
      ::~ SingletonCanvas @H.ToMarkup span
    where span = H.span (H.toMarkup body)

instance H.ToMarkup IconList where
  toMarkup (IconList elems) =
    (H.ul ! A.class_ "c-icon-list") . mconcat $ H.toMarkup <$> elems
