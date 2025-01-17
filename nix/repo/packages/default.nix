{
  inputs,
  cell,
}: let
  inherit (inputs.nixpkgs) lib buildGoModule callPackage fetchfromGithub;
in rec {
  updatenixpkgs = callPackage ./updatenixpkgs {};
}
