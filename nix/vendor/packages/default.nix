{
  inputs,
  cell,
}:
let
  pkgs-22-11 = import inputs.nixos-22-11 {
    inherit (inputs.nixpkgs) system;
  };
  inherit (inputs.nixpkgs) callPackage;
in
rec {
  atc-mi-thermometer-exporter = callPackage ./atc-mi-thermometer-exporter.nix { };
  gyro2bb = callPackage ./gyro2bb.nix { };
  keepassx-22-11 = pkgs-22-11.callPackage (inputs.nixos-22-11 + /pkgs/applications/misc/keepass) { };
  # python-rtimu = callPackage ./python-rtimu.nix { rtimu = rtimu; };
  python-sense-hat = callPackage ./python-sense-hat.nix { rtimu = rtimu; };
  rtimu = callPackage ./rtimu.nix { };
}
