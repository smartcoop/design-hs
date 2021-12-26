module Examples.Form
  ( formGroups
  ) where

import qualified Smart.Html.Checkbox           as C
import qualified Smart.Html.Form               as F
import qualified Smart.Html.Textarea           as TA

formGroups :: [F.FormGroup]
formGroups = checkboxGroups <> textareaGroups

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

textareaGroups :: [F.FormGroup]
textareaGroups = [F.TextareaGroup textareas]
 where
  textareas =
    [ ("Textarea0", TA.Textarea 2 "Textarea0")
    , ("Textarea1", TA.Textarea 5 "Textarea1")
    , ("Textarea2", TA.Textarea 3 "Textarea3")
    ]
