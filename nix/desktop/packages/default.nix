{
  inputs,
  cell,
}:
let
  inherit (inputs.nixpkgs) callPackage;
  inherit (inputs.cells.vendor.packages) eyaml;
  pkgs = import inputs.nixpkgs {
    inherit (inputs.nixpkgs) system;
  };
  inherit (inputs.std.lib.ops) writeScript;
in
{
  backup = callPackage ./backup.nix { inherit writeScript pkgs; };
  checkghtoken = callPackage ./checkghtoken.nix { inherit pkgs; };
  gcd = callPackage ./gcd.nix { inherit writeScript pkgs; };
  gco = callPackage ./gco.nix { inherit pkgs; };
  mpvdash = callPackage ./mpvdash.nix { inherit writeScript pkgs; };
  mvb2sum = callPackage ./mvb2sum.nix { inherit pkgs; };
  sendkeys = callPackage ./sendkeys.nix { inherit writeScript pkgs eyaml; };
}
