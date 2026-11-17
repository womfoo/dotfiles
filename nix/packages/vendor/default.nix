{
  inputs,
  pkgs,
  system,
  ...
}:
let
  pkgs-22-11 = import inputs.nixos-22-11 {
    inherit system;
  };
  pkgs-25-05 = import inputs.nixos-25-05 {
    inherit system;
    config.allowUnfree = true;
  };
  pkgs-25-11 = import inputs.nixos-25-11 {
    inherit system;
    config.allowUnfree = true;
  };
  inherit (pkgs) callPackage;
in
rec {
  atc-mi-thermometer-exporter = callPackage ./atc-mi-thermometer-exporter.nix { };
  # darktable-25-05 = pkgs-25-05.darktable;
  eyaml = callPackage ./hiera-eyaml { };
  git-recover = callPackage ./git-recover.nix { };
  gyro2bb = callPackage ./gyro2bb.nix { };
  keepassx-22-11 =
    if pkgs-22-11.stdenv.isLinux then
      pkgs-22-11.callPackage (inputs.nixos-22-11 + /pkgs/applications/misc/keepass) { }
    else
      pkgs-22-11.hello; # FIXME: dummy just to make things run
  mail-mcp = callPackage ./mail-mcp.nix { };
  murmure = callPackage ./murmure.nix { };
  openp2s = callPackage ./openp2s { };
  inherit (openp2s) openvpn-openp2s;
  parcellite-25-05 = pkgs-25-05.parcellite;
  openvpn3-25-11 = pkgs-25-11.openvpn3;
  # python-rtimu = callPackage ./python-rtimu.nix { rtimu = rtimu; };
  cli-microsoft365 = callPackage ./cli-microsoft365 { };
}
# raspberry pi sense hat, only useful on the pis
// pkgs.lib.optionalAttrs pkgs.stdenv.hostPlatform.isAarch64 rec {
  python-sense-hat = pkgs.callPackage ./python-sense-hat.nix { inherit rtimu; };
  rtimu = pkgs.callPackage ./rtimu.nix { };
}
