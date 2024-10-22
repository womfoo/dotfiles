{
  bee.system = "aarch64-linux";
  bee.pkgs = import inputs.nixos {
    inherit (inputs.nixpkgs) system;
    overlays = [
      cell.overlays.dt_ao_overlay
    ];
  };
  imports = [
    inputs.nixos-generators.nixosModules.sd-aarch64
    cell.nixosModules.common
    cell.nixosModules.gikos-kranium
    cell.nixosModules.wizkell
  ];
  networking.hostName = "dreadfort";
  system.stateVersion = "24.11";

  # services.getty.autologinUser = lib.mkForce "root";

  environment.systemPackages = with pkgs; [
    # inputs.cells.iot.packages.wizkell
    git
  ];

  # sdImage = {
  #   compressImage = false;
  # };

}
