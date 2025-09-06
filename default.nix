let
  overlay = final: prev: {
    haskell = prev.haskell // {
      packageOverrides =
        hfinal: hprev:
        prev.haskell.packageOverrides hfinal hprev
        // {
          aoc2024 = hfinal.callCabal2nix "aoc2024" ./2024 { };
        };
    };
    aoc2024 = final.haskell.lib.compose.justStaticExecutables final.haskellPackages.aoc2024;
  };
  pkgs = import <nixpkgs> { overlays = [ overlay ]; };
  hPkgs = pkgs.haskellPackages;
in

hPkgs.shellFor {
  withHoogle = true;

  packages = p: [ p.aoc2024 ];

  buildInputs = with pkgs; [
    hPkgs.cabal-install
    hPkgs.haskell-language-server
    hPkgs.fourmolu
    hPkgs.cabal-fmt
    hPkgs.hlint
    hPkgs.regex-tdfa # not included in snapshot by default
    yaml-language-server
    just
    aoc-cli
  ];
}
