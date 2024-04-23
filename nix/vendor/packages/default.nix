{ inputs, cell }:
rec {
  # https://github.com/NixOS/nixpkgs/pull/209778
  openlens = inputs.nixpkgs.callPackage ./openlens.nix { };

  rtimu = inputs.nixpkgs.callPackage ./rtimu.nix { };

  # python-rtimu = inputs.nixpkgs.callPackage ./python-rtimu.nix { rtimu = rtimu; };

  python-sense-hat = inputs.nixpkgs.callPackage ./python-sense-hat.nix { rtimu = rtimu; };

  # keepassx = inputs.nixpkgs.callPackage ./keepassx {};
}
