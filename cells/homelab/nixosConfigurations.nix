{
  vhagar = {pkgs, ...}: {
    bee.system = "x86_64-linux";
    bee.pkgs = import inputs.nixos {
      inherit (inputs.nixpkgs) system;
      config.allowBroken = true;
      config.allowUnfree = true;
      overlays = [ inputs.nur.overlay
                   cell.overlays.x86_64
                   # inputs.home-manager.nixosModule # FIXME
                   # inputs.sops-nix.nixosModules.sops # FIXME
                 ];
    };
    imports = [
      cell.nixosModules.common
      cell.nixosModules.builder
      cell.nixosModules.desktop-apps
      cell.nixosModules.gikos-kranium
      # cell.nixosModules.gikos-kranium-hm # FIXME
      cell.hardwareProfiles.vhagar
      ./vhagar/configuration.nix
    ];
    environment.systemPackages = [
      inputs.cells.vendor.packages.openlens
    ];
  };

  waycastle = {pkgs, ...}: {
    bee.system = "x86_64-linux";
    bee.pkgs = import inputs.nixos {
      inherit (inputs.nixpkgs) system;
      config.allowUnfree = true;
    };
    imports = [
      cell.hardwareProfiles.waycastle
      cell.nixosModules.router
      ./waycastle/configuration.nix
    ];
    services.router.enable = true;
  };

}
