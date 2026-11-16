{
  pkgs,
  config,
  inputs,
  inputs',
  self,
  secrets,
  ...
}:
{
  environment.systemPackages = with pkgs; [
    darcs
    wget
  ];
  imports = [
    ./hardware.nix
    self.nixosModules.common
    self.nixosModules.gikos-kranium
    self.nixosModules.wireguard
    secrets."wg-stonedoor-priv-key".nixosModule
    inputs.srvos.nixosModules.server
    inputs.srvos.nixosModules.mixins-nginx
    inputs'.kraniumau.cells.site.nixosModules.default
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
  services.mywg.hostPrivKeyFile = secrets."wg-stonedoor-priv-key".path config;
  services.mywg.makeServer = true;
  services.mywg.peer = "waycastle";
  # services.mywg.peers = [ "waycastle" ];
  services.nginx.virtualHosts = {
    "au01.gikos.net" = {
      forceSSL = true;
      enableACME = true;
      globalRedirect = "https://gikos.net";
    };
  };
  services.sshguard.enable = true;
  system.stateVersion = "24.11";

}
