module Examples.Form
  ( formGroups
  ) where

import qualified Smart.Html.Checkbox           as C
import qualified Smart.Html.Form               as F

formGroups :: [F.FormGroup]
formGroups = checkboxGroups -- add more form groups here. 

checkboxGroups :: [F.FormGroup]
checkboxGroups =
  [ F.CheckboxGroup "A group of checkboxes" checkboxes
  , F.CheckboxGroupInline "An inline group of checkboxes" checkboxes
  ]
 where
  checkboxes =
    [ C.CheckboxEnabled (Just "someId") C.Unchecked "Unchecked"
    , C.CheckboxEnabled (Just "someId") C.Checked "Checked"
    , C.CheckboxDisabled (Just "someId") C.Unchecked "Unchecked"
    , C.CheckboxDisabled (Just "someId") C.Checked "Checked"
    ]

