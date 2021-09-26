self: super:
let

  lib = super.lib;
  sources = import ./sources.nix;

  inherit (import sources.gitignore { inherit lib; }) gitignoreSource;

  ourOverrides = selfh: superh: {
    the-smart-designer =
      selfh.callCabal2nix "the-smart-designer" (gitignoreSource ../lib) { };
  };

in {
  haskellPackages = super.haskellPackages.override (old: {
    overrides =
      lib.composeExtensions (old.overrides or (_: _: { })) ourOverrides;
  });
}
