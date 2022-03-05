module Examples.Navbar
  ( navbars
  , exampleNavbar
  ) where

import           Smart.Html.Navbar

navbars :: [Navbar]
navbars =
  [ exampleNavbar
  ]

exampleNavbar :: Navbar
exampleNavbar = Navbar
  [ Entry "Activities" (Link "#")
  , Entry "Management" (SubEntries
      [ SubEntry "Nav item" "#"
      , SubEntry "Nav item" "#"
      , SubEntry "Nav item" "#"
      ])
  , Entry "Documents" (SubEntries
      [ SubEntry "Nav item" "#"
      , SubEntry "Nav item" "#"
      ])
  , Entry "Members" (Link "#")
  , Entry "Archive" (Link "#")
  ]
