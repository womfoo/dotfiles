{ config, pkgs, lib, ... }: {
  environment.systemPackages = with pkgs; [ darcs vim wget ];
  networking.domain = "gikos.net";
  networking.firewall.allowedTCPPorts = [ 80 443 ];
  networking.hostName = "stonedoor";
  networking.interfaces.enp1s0.useDHCP = true;
  networking.useDHCP = false;
  security.acme.acceptTerms = true;
  security.acme.defaults.email = "kranium@gikos.net";
  services.fail2ban.enable = true;
  services.fail2ban.jails.ssh-iptables = "enabled = true";
  services.fail2ban.maxretry = 1;
  services.nginx.enable = true;
  services.nginx.recommendedProxySettings = true;
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
  services.openssh.gatewayPorts = "yes";
  services.openssh.extraConfig = ''
    AllowTcpForwarding yes
  '';
}
