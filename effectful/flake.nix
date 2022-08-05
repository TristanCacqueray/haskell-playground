{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/ed014c27f4d0ca772fb57d3b8985b772b0503bbd";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        effectful-src = builtins.fetchGit {
          url = "https://github.com/haskell-effectful/effectful";
          ref = "master";
          rev = "5c5da07678816053bef105494f32b49435eef53a";
        };

        pkgs = import nixpkgs { inherit system; };
        packageName = "effectful-playground";

        haskellOverrides = {
          overrides = hpFinal: hpPrev: {
            effectful-core = hpPrev.callCabal2nix "effectful-core" "${effectful-src}/effectful-core" {};
            effectful = hpPrev.callCabal2nix "effectful" "${effectful-src}/effectful" {};
          };
        };

        haskellPackages =
          pkgs.haskell.packages.ghc922.override haskellOverrides;
        myPackage =
          haskellPackages.callCabal2nix packageName "${self}/effectful" { };

        exe = pkgs.haskell.lib.justStaticExecutables myPackage;

      in {
        apps.default = exe;
        packages.default = myPackage;

        devShell = haskellPackages.shellFor {
          packages = p: [ myPackage ];

          buildInputs = with haskellPackages; [
            ghcid
            ormolu
            cabal-install
            hlint
            pkgs.haskell-language-server
          ];
        };

        devShells.hoogle = haskellPackages.shellFor {
          packages = p: [ myPackage ];
          withHoogle = true;
        };
      });
}
