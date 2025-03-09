{ pkgs, ... }:
{
  bee.system = "x86_64-linux";
  bee.pkgs = import inputs.srvos.inputs.nixpkgs { inherit (inputs.nixpkgs) system; };
  environment.systemPackages = with pkgs; [
    darcs
    wget
  ];
  imports = [
    cell.hardwareProfiles.stonedoor
    cell.nixosModules.common
    cell.nixosModules.gikos-kranium
    cell.nixosModules.wireguard
    cell.secrets."wg-stonedoor-priv-key".nixosModule
    inputs.srvos.nixosModules.server
    inputs.srvos.nixosModules.mixins-nginx
  ];
  networking.domain = "gikos.net";
  networking.firewall.allowedTCPPorts = [
    80
    443
  ];
  networking.hostName = "stonedoor";
  networking.interfaces.enp1s0.useDHCP = true;
  networking.useDHCP = false;
  security.acme.acceptTerms = true;
  security.acme.defaults.email = "kranium@gikos.net";
  services.mywg.enable = true;
  services.mywg.host = "stonedoor";
  services.mywg.hostPrivKeyFile = cell.secrets."wg-stonedoor-priv-key".path config;
  services.mywg.makeServer = true;
  services.mywg.peer = "waycastle";
  # services.mywg.peers = [ "waycastle" ];
  services.nginx.virtualHosts = {
    "gikos.net" = {
      forceSSL = true;
      enableACME = true;
      locations."/".root = "/srv/gikos.net";
    };
    "www.gikos.net" = {
      forceSSL = true;
      enableACME = true;
      globalRedirect = "https://gikos.net";
    };
    "au01.gikos.net" = {
      forceSSL = true;
      enableACME = true;
      globalRedirect = "https://gikos.net";
    };
  };
  services.sshguard.enable = true;
  system.stateVersion = "24.11";
}
