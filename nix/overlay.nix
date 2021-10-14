self: super:
let

  lib = super.lib;
  sources = import ./sources.nix;
  contents = import ./contents.nix { nixpkgs = super; };

  inherit (super.lib.attrsets) mapAttrs;
  inherit (import sources.gitignore { inherit lib; }) gitignoreSource;

  ourOverrides = selfh: superh:
    mapAttrs (name: dir:
      selfh.callCabal2nix "${name}" (gitignoreSource dir) { }) contents.pkgList;

in {
  haskellPackages = super.haskellPackages.override (old: {
    overrides =
      lib.composeExtensions (old.overrides or (_: _: { })) ourOverrides;
  });
}
