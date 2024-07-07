{ pkgs, ... }:
{
  bee.system = "x86_64-linux";
  bee.pkgs = import inputs.nixos-24-05 {
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
  # services.router.config.wan.interface = "tun0";
  services.router.enable = true;
  system.stateVersion = "24.05";
  environment.systemPackages = with pkgs; [ tmux picocom ];

  # services.openvpn.servers = {
  #   mullvadUSA  = {
  #     autoStart = true;
  #     config = '' config /srv/mullvad_config_linux_us_sjc/mullvad_us_sjc.conf ''; };
  # };

}
