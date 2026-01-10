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
  bee.pkgs = import inputs.nixos-25-11 {
    inherit (inputs.nixpkgs.hostPlatform) system;
    config.allowUnfree = true;
  };
  hardware.cpu.intel.updateMicrocode = true;
  hardware.enableAllFirmware = true;
  hardware.enableAllHardware = true;
  # hardware.enableRedistributableFirmware = true;
  imports = [
    cell.hardwareProfiles.waycastle
    cell.nixosModules.common
    cell.nixosModules.gikos-kranium
    cell.nixosModules.router
    cell.nixosModules.wireguard
    cell.secrets."wg-waycastle-priv-key".nixosModule
    inputs.cells.iot.nixosModules.mi-temp-exporter
    inputs.srvos.nixosModules.mixins-telegraf
    inputs.srvos.nixosModules.roles-prometheus
    inputs.srvos.nixosModules.server
  ];
  networking.firewall.interfaces.enp0s31f6.allowedUDPPorts = [
    51820
    8581
  ];
  networking.firewall.interfaces.enp0s20f0u4u2.allowedTCPPorts = [
    9090
    8581
  ];
  networking.firewall.interfaces.wlp2s0.allowedTCPPorts = [ 8581 ];

  networking.hostName = "waycastle";
  networking.nat.forwardPorts = [
    {
      sourcePort = 24800;
      destination = "172.19.86.101:24800";
    }
  ];
  services.fwupd.enable = true;
  services.locate.enable = true;
  services.mi-temp-exporter.enable = true;
  services.mi-temp-exporter.macs = [
    temp1.interfaces.bt.mac
    temp2.interfaces.bt.mac
  ];
  services.mywg.enable = true;
  services.mywg.host = "waycastle";
  services.mywg.hostPrivKeyFile = cell.secrets."wg-waycastle-priv-key".path config;
  services.mywg.peer = "stonedoor";
  # services.mywg.peers = [ "stonedoor" ];
  # FIXME: try to hook this after online
  systemd.services.prometheus.serviceConfig = {
    RestartSec = 5;
    Restart = "always";
  };
  services.prometheus.extraFlags = [
    "--storage.tsdb.retention.time=360d"
  ];
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
            "${constants.devices.dreamfyre.interfaces.lan.ip}:9273"
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
  services.router.inventory = builtins.removeAttrs constants.devices [ "wizbulb3" ]; # bulb @ diff AP
  services.sshguard.enable = true;
  services.telegraf.extraConfig.inputs.internet_speed = { };
  services.telegraf.extraConfig.inputs.net = { };
  users.mutableUsers = pkgs.lib.mkForce true;
  system.stateVersion = "24.05";

  services.homebridge.enable = true;
  services.homebridge.openFirewall = true;

  security.sudo.execWheelOnly = pkgs.lib.mkForce false;

}
