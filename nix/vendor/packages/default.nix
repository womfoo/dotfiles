{
  inputs,
  cell,
}:
let
  pkgs-22-05 = import inputs.nixos-22-05 {
    inherit (inputs.nixpkgs) system;
  };
  pkgs-22-11 = import inputs.nixos-22-11 {
    inherit (inputs.nixpkgs) system;
  };
  pkgs-25-05 = import inputs.nixos-25-05 {
    inherit (inputs.nixpkgs) system;
    config.allowUnfree = true;
  };
  inherit (inputs.nixpkgs) callPackage;
in
rec {
  atc-mi-thermometer-exporter = callPackage ./atc-mi-thermometer-exporter.nix { };
  eyaml = callPackage ./hiera-eyaml { };
  git-recover = callPackage ./git-recover.nix { };
  gyro2bb = callPackage ./gyro2bb.nix { };
  darktable-25-05 = pkgs-25-05.darktable;
  gimp-25-05 = pkgs-25-05.gimp;
  ollama-25-05 = pkgs-25-05.ollama;
  open-webui-25-05 = pkgs-25-05.open-webui;
  salt-22-05 = pkgs-22-05.salt;
  ansible-22-05 = pkgs-22-05.ansible;
  vagrant-25-05 = pkgs-25-05.vagrant;
  parcellite-25-05 = pkgs-25-05.parcellite;
  keepassx-22-11 =
    if pkgs-22-11.stdenv.isLinux then
      pkgs-22-11.callPackage (inputs.nixos-22-11 + /pkgs/applications/misc/keepass) { }
    else
      pkgs-22-11.hello; # FIXME: dummy just to make things run
  # python-rtimu = callPackage ./python-rtimu.nix { rtimu = rtimu; };
  python-sense-hat = callPackage ./python-sense-hat.nix { rtimu = rtimu; };
  rtimu = callPackage ./rtimu.nix { };

  /*
    Authentication failed. You can return to the application. Feel free to close this browser tab.

    Error details: error invalid_client error_description: AADSTS650057: Invalid resource. The client has requested access to a resource which is not listed in the requested permissions in the client's application registration. Client app ID: c632b3df-fb67-4d84-bdcf-b95ad541b5c8(Azure VPN). Resource value from request: 41b23e61-6c1e-4545-b367-cd054e0ed4b4. Resource app ID: 41b23e61-6c1e-4545-b367-cd054e0ed4b4. List of valid resources from app registration: . Trace ID: 5d493471-885b-4af8-a409-f849da6f0500 Correlation ID: 64b39a80-215d-43ce-af9f-7968d1da80fe Timestamp: 2025-08-12 14:24:22Z
  */

  azurevpn = callPackage (inputs.azurevpn + "/azurevpn.nix") { };
}
