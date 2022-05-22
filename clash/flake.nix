{
  description = "Clash Playground";
  nixConfig.bash-prompt = "[nix(clash)] ";

  inputs = { nixpkgs.url = "github:NixOS/nixpkgs"; };

  outputs = { self, nixpkgs }:
    let
      pkgs = import nixpkgs { localSystem = "x86_64-linux"; };
      compilerVersion = "8107";
      compiler = "ghc" + compilerVersion;
      hsPkgs = pkgs.haskell.packages.${compiler}.override {
        overrides = hpFinal: hpPrev:
          let
            unbreak = d: pkgs.haskell.lib.overrideCabal d { broken = false; };
            dontCheck = pkgs.haskell.lib.dontCheck;
          in {
            doctest-parallel = dontCheck (unbreak hpPrev.doctest-parallel);
            clash-prelude = dontCheck hpPrev.clash-prelude;
            tasty-hedgehog = hpPrev.tasty-hedgehog_1_2_0_0;

            retroclash-lib = (hpPrev.callCabal2nix "retroclash-lib"
              (builtins.fetchGit {
                url = "https://github.com/gergoerdi/retroclash-lib";
                ref = "q-monoid";
                rev = "c62f0adfab5cf39e2de0133b1960385d607968ee";
              }) { });
          };
      };
      pkg = hsPkgs.callCabal2nix "clash-playground" "${self}/clash" { };

    in {
      devShell."x86_64-linux" = hsPkgs.shellFor {
        packages = p: [ pkg ];
        extraDependencies = p: {
          libraryHaskellDepends = [ p.clash-shake p.pretty-simple ];
        };
        buildInputs = [ pkgs.cabal-install pkgs.haskell-language-server ];
      };
    };
}
