# My Haskell playground

This repository contains code sample and small project to explore the [Haskell](https://haskell.org) language.

Get haskell packages using nix:

```
PKGS="wai warp http-types"
PIN=98747f27ecfee70c8c97b195cbb94df80a074dda
nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/${PIN}.tar.gz -p "haskellPackages.ghcWithPackages(p: with p; [$PKGS])"
```
