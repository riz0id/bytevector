{ compiler ? "ghc921" }:

let
  nixpkgs = builtins.fetchTarball {
    # nixpkgs release 21.11
    # url: <https://github.com/NixOS/nixpkgs/releases/tag/21.11>
    url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/21.11.tar.gz";
    sha256 = "162dywda2dvfj1248afxc45kcrg83appjd0nmdb541hl7rnncf02";
  };

  config = { };

  overlay = pkgsNew: pkgsOld: {
    haskell = pkgsOld.haskell // {
      packages = pkgsOld.haskell.packages // {
        "${compiler}" = pkgsOld.haskell.packages."${compiler}".override (old: {
          overrides = let

            packageSources =
              let
                unlifted-bool = builtins.fetchTarball {
                  # unlifted-bool-1.0.0
                  url    = "https://github.com/riz0id/unlifted-bool/releases/download/1.0.0/unlifted-bool-1.0.0.tar.gz";
                  sha256 = "0xv3rd5j3njz76kdla86i0yjgv61fic2qx1pp2bn35r21bl2flxr";
                };

               in pkgsNew.haskell.lib.packageSourceOverrides {
                 "bytevector"    = ./.;
                 "unlifted-bool" = unlifted-bool;
               };

            manualOverrides = haskellPackagesNew: haskellPackagesOld: { };

            default = old.overrides or (_: _: { });

          in pkgsNew.lib.fold pkgsNew.lib.composeExtensions default [
            packageSources
            manualOverrides
          ];
        });
      };
    };
  };

  pkgs = import nixpkgs {
    inherit config;
    overlays = [ overlay ];
  };

in {
  inherit (pkgs.haskell.packages."${compiler}") bytevector;

  shell = (pkgs.haskell.packages."${compiler}".bytevector).env;
}
