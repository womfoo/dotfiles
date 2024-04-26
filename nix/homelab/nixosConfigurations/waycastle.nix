{ pkgs, ... }:
{
  bee.system = "x86_64-linux";
  bee.pkgs = import inputs.nixos {
    inherit (inputs.nixpkgs) system;
    allowUnfree = true;
  };
  hardware.enableRedistributableFirmware = true;
  imports = [
    cell.hardwareProfiles.waycastle
    cell.nixosModules.common
    cell.nixosModules.gikos-kranium
    cell.nixosModules.router
  ];
  networking.hostName = "waycastle";
  services.fwupd.enable = true;
  services.router.config.passwordFile = inputs.lihim.x86_64-linux.lihim.lib.mkHostApdPasswordFile;
  services.router.enable = true;
  system.stateVersion = "24.05";
  environment.systemPackages = with pkgs; [ tmux picocom ];
}
