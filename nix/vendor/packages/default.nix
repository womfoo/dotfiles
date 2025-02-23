{
  inputs,
  cell,
}:
let
  pkgs-22-11 = import inputs.nixos-22-11 {
    inherit (inputs.nixpkgs) system;
  };
in
rec {
  gyro2bb = inputs.nixpkgs.callPackage ./gyro2bb.nix { };
  keepassx-22-11 = pkgs-22-11.callPackage (inputs.nixos-22-11 + /pkgs/applications/misc/keepass) { };
  # python-rtimu = inputs.nixpkgs.callPackage ./python-rtimu.nix { rtimu = rtimu; };
  python-sense-hat = inputs.nixpkgs.callPackage ./python-sense-hat.nix { rtimu = rtimu; };
  rtimu = inputs.nixpkgs.callPackage ./rtimu.nix { };
}
