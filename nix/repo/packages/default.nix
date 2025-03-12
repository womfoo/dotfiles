{
  inputs,
  cell,
}:
let
  inherit (inputs.nixpkgs)
    lib
    buildGoModule
    callPackage
    fetchfromGithub
    ;
in
rec {
  updatenixpkgs = callPackage ./updatenixpkgs { };
  gen-secrets-nix = inputs.std.lib.ops.writeScript {
    name = "gen-secrets-nix";
    text = ./gen-secrets-nix.sh;
  };
}
