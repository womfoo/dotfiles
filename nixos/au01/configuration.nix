{ config, pkgs, ... }:
{
  deployment.targetHost = "149.28.180.243";
  # deployment.buildOnTarget = true; # requires colmena unstable

  imports = [
    ./hardware-configuration.nix
    ../shared/gikos-kranium.nix
  ];

  boot.loader.grub.device = "/dev/vda";
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;

  networking.hostName = "au01";
  networking.domain = "gikos.net";
  networking.useDHCP = false;
  networking.interfaces.enp1s0.useDHCP = true;

  services.fail2ban.enable = true;
  services.fail2ban.jails.ssh-iptables = "enabled = true";

  services.nginx.enable = true;
  services.nginx.virtualHosts = {
    "gikos.net"      = { forceSSL = true; enableACME = true; locations."/".root = "/srv/gikos.net"; };
    "www.gikos.net"  = { forceSSL = true; enableACME = true; globalRedirect = "https://gikos.net"; };
    "au01.gikos.net" = { forceSSL = true; enableACME = true; globalRedirect = "https://gikos.net"; };
  };

  security.acme.acceptTerms = true;
  security.acme.defaults.email = "kranium@gikos.net";

  environment.systemPackages = with pkgs; [
    darcs
    vim
    wget
  ];

  services.openssh.enable = true;
  services.openssh.gatewayPorts = "yes";
  services.openssh.extraConfig = ''
    AllowTcpForwarding yes
  '';
  # services.openssh.permitRootLogin = "no";

  networking.firewall.allowedTCPPorts = [ 80 443 ];
  # networking.firewall.allowedUDPPorts = [ ... ];

}
