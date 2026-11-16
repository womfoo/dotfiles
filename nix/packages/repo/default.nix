{
  pkgs,
  writeScript,
  ...
}:
let
  inherit (pkgs) callPackage;
in
rec {
  updatenixpkgs = callPackage ./updatenixpkgs { };
  gen-secrets-nix = writeScript {
    name = "gen-secrets-nix";
    text = ./gen-secrets-nix.sh;
  };
}
