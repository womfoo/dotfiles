{ pkgs, config, ... }:
let
  inherit (inputs.lihim.lihim) constants lib;
in
{
  bee.system = "x86_64-linux";
  # bee.pkgs = import inputs.srvos.inputs.nixpkgs {
  bee.pkgs = import inputs.nixos-24-05 {
    inherit (inputs.nixpkgs) system;
    allowUnfree = true;
  };
  hardware.cpu.intel.updateMicrocode = true;
  hardware.enableRedistributableFirmware = true;
  imports = [
    cell.hardwareProfiles.waycastle
    cell.nixosModules.common
    cell.nixosModules.gikos-kranium
    cell.nixosModules.router
    cell.nixosModules.wireguard
    cell.secrets."wg-waycastle-priv-key"
    inputs.srvos.nixosModules.server
    inputs.srvos.nixosModules.roles-prometheus
  ];
  networking.firewall.interfaces.enp0s31f6.allowedUDPPorts = [ 51820 ];
  networking.hostName = "waycastle";
  services.fwupd.enable = true;
  services.router.config.passwordFile = lib.mkHostApdPasswordFile;
  # services.router.config.wan.interface = "wg0";
  # services.router.config.wan.interface = "tun0";
  # services.router.config.wan.interface = "eth0"; # iphone backup
  services.router.config.wireless.interface = "wlp2s0";
  services.router.enable = true;
  services.router.inventory = constants.devices;
  environment.systemPackages = with pkgs; [ picocom ];
  services.mywg.enable = true;
  services.mywg.host = "waycastle";
  services.mywg.hostPrivKeyFile = config.age.secrets."wg-waycastle-priv-key".path;
  services.mywg.peer = "stonedoor";
  # services.mywg.peers = [ "stonedoor" ];
  # services.prometheus.globalConfig.scrape_interval = "10s"; # "1m"
  services.prometheus.scrapeConfigs = [
    {
      job_name = "node";
      static_configs = [{
        targets = [ "${constants.devices.vhagar.interfaces.lan.mac}:9273" ];
      }];
    }
  ];
  services.sshguard.enable = true;
  # users.mutableUsers = inputs.nixpkgs.lib.mkForce true;
  users.mutableUsers = pkgs.lib.mkForce true;
}
