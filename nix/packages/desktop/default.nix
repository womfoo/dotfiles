{
  pkgs,
  eyaml,
  writeScript,
  ...
}:
let
  inherit (pkgs) callPackage;
in
{
  backup = callPackage ./backup.nix { inherit writeScript pkgs; };
  checkghtoken = callPackage ./checkghtoken.nix { inherit pkgs; };
  gcd = callPackage ./gcd.nix { inherit writeScript pkgs; };
  gco = callPackage ./gco.nix { inherit pkgs; };
  gitshuffle = callPackage ./gitshuffle.nix { inherit pkgs; };
  mpvdash = callPackage ./mpvdash.nix { inherit writeScript pkgs; };
  mvb2sum = callPackage ./mvb2sum.nix { inherit pkgs; };
  sendkeys = callPackage ./sendkeys.nix { inherit writeScript pkgs eyaml; };
  scanpdf = callPackage ./scanpdf.nix { inherit writeScript pkgs; };
}
