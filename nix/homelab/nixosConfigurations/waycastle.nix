{
  pkgs,
  config,
  ...
}:
let
  inherit (inputs.lihim.lihim) constants lib;
  inherit (inputs.lihim.lihim.constants.devices) temp1 temp2;
in
{
  bee.system = "x86_64-linux";
  bee.pkgs = import inputs.nixos-24-05 {
    inherit (inputs.nixpkgs) system;
    allowUnfree = true;
  };
  hardware.bluetooth.enable = true;
  hardware.cpu.intel.updateMicrocode = true;
  hardware.enableAllFirmware = true;
  # hardware.enableRedistributableFirmware = true;
  imports = [
    cell.hardwareProfiles.waycastle
    cell.nixosModules.common
    cell.nixosModules.gikos-kranium
    cell.nixosModules.router
    cell.nixosModules.wireguard
    cell.secrets."wg-waycastle-priv-key"
    inputs.cells.iot.nixosModules.mi-temp-exporter
    inputs.srvos.nixosModules.mixins-telegraf
    inputs.srvos.nixosModules.roles-prometheus
    inputs.srvos.nixosModules.server
  ];
  networking.firewall.interfaces.enp0s31f6.allowedUDPPorts = [ 51820 ];
  networking.firewall.interfaces.enp0s20f0u3u2.allowedTCPPorts = [ 9090 ];
  networking.hostName = "waycastle";
  services.fwupd.enable = true;
  services.mi-temp-exporter.enable = true;
  services.mi-temp-exporter.macs = [
    temp1.interfaces.bt.mac
    temp2.interfaces.bt.mac
  ];
  services.mywg.enable = true;
  services.mywg.host = "waycastle";
  services.mywg.hostPrivKeyFile = config.age.secrets."wg-waycastle-priv-key".path;
  services.mywg.peer = "stonedoor";
  # services.mywg.peers = [ "stonedoor" ];
  # services.prometheus.globalConfig.scrape_interval = "10s"; # "1m"
  services.prometheus.listenAddress = constants.devices.waycastle.interfaces.lan.ip;
  services.prometheus.scrapeConfigs = [
    {
      job_name = "node";
      static_configs = [
        {
          targets = [
            "127.0.0.1:9273"
            "127.0.0.1:9977"
            "${constants.devices.vhagar.interfaces.lan.ip}:9273"
          ];
        }
      ];
    }
  ];
  services.router.config.passwordFile = lib.mkHostApdPasswordFile;
  # services.router.config.wan.interface = "wg0";
  # services.router.config.wan.interface = "tun0";
  # services.router.config.wan.interface = "eth0"; # iphone backup
  services.router.config.wireless.interface = "wlp2s0";
  services.router.enable = true;
  services.router.inventory = constants.devices;
  services.sshguard.enable = true;
  # users.mutableUsers = inputs.nixpkgs.lib.mkForce true;
  users.mutableUsers = pkgs.lib.mkForce true;
}
