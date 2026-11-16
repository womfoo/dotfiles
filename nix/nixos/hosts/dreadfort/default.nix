{
  pkgs,
  inputs,
  self,
  ...
}:
{
  nixpkgs.overlays = [
    self.overlays.dt_ao_overlay
  ];
  imports = [
    inputs.nixos-generators.nixosModules.sd-aarch64
    self.nixosModules.common
    self.nixosModules.gikos-kranium
    self.nixosModules.wizkell
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
