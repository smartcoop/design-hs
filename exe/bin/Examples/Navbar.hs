module Examples.Navbar
  ( navbars
  , exampleNavbar
  , exampleNavbarWebsite
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

exampleNavbarWebsite :: NavbarWebsite
exampleNavbarWebsite = NavbarWebsite
  [ Entry "Design" (SubEntries
      [ SubEntry "Nav item" "#"
      , SubEntry "Nav item" "#"
      , SubEntry "Nav item" "#"
      ])
  , Entry "Development" (SubEntries
      [ SubEntry "Nav item" "#"
      , SubEntry "Nav item" "#"
      ])
  , Entry "Components" (Link "#")
  , Entry "Blog" (Link "#")
  , Entry "Changelog" (Link "#")
  ]
