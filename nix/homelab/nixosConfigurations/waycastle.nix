{ pkgs, ... }:
let
  inherit (inputs.lihim.lihim) constants lib;
in
{
  bee.system = "x86_64-linux";
  bee.pkgs = import inputs.srvos.inputs.nixpkgs {
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
    inputs.srvos.nixosModules.server
    inputs.srvos.nixosModules.roles-prometheus
  ];
  networking.firewall.interfaces.enp0s20f0u3u2.allowedTCPPorts = [ 80 9090 ];
  networking.firewall.interfaces.enp0s31f6.allowedUDPPorts = [ 51820 ];
  networking.firewall.interfaces.enp0s31f6.allowedTCPPorts = [ 80 ];
  networking.hostName = "waycastle";
  services.fwupd.enable = true;
  services.router.config.passwordFile = lib.mkHostApdPasswordFile;
  # services.router.config.wan.interface = "tun0";
  # services.router.config.wan.interface = "eth0"; # iphone backup
  services.router.config.wireless.interface = "wlp2s0";
  services.router.enable = true;
  services.router.inventory = constants.devices;
  environment.systemPackages = with pkgs; [ picocom ];
  # services.prometheus.globalConfig.scrape_interval = "10s"; # "1m"
  services.prometheus.scrapeConfigs = [
    {
      job_name = "node";
      static_configs = [{
        targets = [ "${constants.devices.vhagar.interfaces.lan.mac}:9273" ];
      }];
    }
  ];
  # services.openvpn.servers = {
  #   mullvadUSA  = {
  #     autoStart = true;
  #     config = '' config /srv/mullvad_config_linux_us_sjc/mullvad_us_sjc.conf ''; };
  # };
  users.users.root.initialHashedPassword = constants.defRootPasswordHash;
}
