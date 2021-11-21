let
  sources = import ./nix/sources.nix;
  overlays = import ./nix/overlays.nix;

  # Vanilla nixpkgs.
  nixpkgs = import sources.nixpkgs {};
  # Nixpkgs with our overlays.
  nixpkgsOverlayed = import sources.nixpkgs { inherit overlays; };
  # Contents, which supply our package names.
  contents = import ./nix/contents.nix { inherit nixpkgs; };

  # Functions we'll need to manipulate nix.
  inherit (nixpkgs.lib.attrsets) getAttrFromPath mapAttrs;

  # We get the haskell package this project produces by name from the overlayed nixpkgs. 
  # The names, again, are supplied from the contents.nix file, no repeating. 
  getHaskellPkg = name: _: getAttrFromPath [ name ] nixpkgsOverlayed.haskellPackages;

in mapAttrs getHaskellPkg contents.pkgList
