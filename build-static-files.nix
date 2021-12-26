let sources = import ./nix/sources.nix;
    # Index is built under outputDir
    # Examples are built under a subdirectory of outputDir
in { outputDir ? "html"
   , examplesSubDir ? "examples"
   , pkgs ? import sources.nixpkgs { }
   , system ? builtins.currentSystem
   }:
let
  # build the static executables first.
  exe =
    with (import ./.);
    pkgs.haskell.lib.justStaticExecutables(design-hs-exe);

in pkgs.stdenv.mkDerivation {

  inherit system;

  name = "design-hs-html";
  src = ./.;
  buildInputs = [ exe ]; 
  installPhase = '' 
    mkdir -p $out/${outputDir} # create the output directory. 
    # The haskell side ensures all subdirectories are created on the fly. 
    ${exe}/bin/design-hs-exe --output-dir $out/${outputDir} --examples-sub-dir $out/${outputDir}/${examplesSubDir}
  '';             

}
