{ inputs, cell }:
let
  pkgs-24-05 = import inputs.nixos-24-05 {
    inherit (inputs.nixpkgs) system;
    config.allowUnfree = true; # vagrant
  };
  pkgs-22-11 = import inputs.nixos-22-11 {
    inherit (inputs.nixpkgs) system;
  };
in
rec {
  keepassx-22-11 = pkgs-22-11.keepassx;
  # python-rtimu = inputs.nixpkgs.callPackage ./python-rtimu.nix { rtimu = rtimu; };
  python-sense-hat = inputs.nixpkgs.callPackage ./python-sense-hat.nix { rtimu = rtimu; };
  rtimu = inputs.nixpkgs.callPackage ./rtimu.nix { };
  vagrant-24-05 = pkgs-24-05.vagrant;
}
