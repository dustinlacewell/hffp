let
  pkgs = import <nixpkgs> { };
in
  (pkgs.haskell.packages.ghc8107.callCabal2nix "project" ./. {}).env.overrideAttrs (old: {
    buildInputs = old.buildInputs ++ [
      pkgs.entr
      pkgs.haskell.packages.ghc8107.ghcid
      pkgs.haskell.packages.ghc8107.ghcide
      pkgs.haskell.packages.ghc8107.haskell-language-server
      pkgs.haskell.packages.ghc8107.hlint
      pkgs.haskell.packages.ghc8107.doctest
    ];
  })
