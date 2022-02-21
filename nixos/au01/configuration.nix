{ config, pkgs, ... }:
{
  deployment.targetHost = "149.28.180.243";
  # deployment.buildOnTarget = true; # requires colmena unstable

  imports = [
    ./hardware-configuration.nix
    ../shared/gikos-kranium.nix
    ../shared/secrets.nix
    ../shared/common.nix
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
    "rc.gikos.net"          = { locations."/" = { proxyPass = "http://10.100.0.3:80"; }; };
    "octoprint.gikos.net"   = { locations."/" = { proxyPass = "http://10.100.0.3:80"; }; };
    "silverspark.gikos.net" = { locations."/" = { proxyPass = "http://10.100.0.2:80"; }; };
    "paperless.gikos.net"   = { locations."/" = { proxyPass = "http://10.100.0.2:80"; }; };

  };

  security.acme.acceptTerms = true;
  security.acme.defaults.email = "kranium@gikos.net";

  environment.systemPackages = with pkgs; [
    darcs
    vim
    wget
  ];

  services.openssh.gatewayPorts = "yes";
  services.openssh.extraConfig = ''
    AllowTcpForwarding yes
  '';
  # services.openssh.permitRootLogin = "no";

  networking.firewall.allowedTCPPorts = [ 80 443 ];
  networking.firewall.allowedUDPPorts = [
    config.networking.wireguard.interfaces.wg0.listenPort
  ];

  networking.wireguard.interfaces = {
    wg0 = {
      ips = [ "10.100.0.1/24" ];
      listenPort = 51820;
      privateKeyFile = config.sops.secrets.wg-private-key.path;
      peers = [
        { # silverspark
          publicKey = "yN3RhSg0wcdGZoqZbrnb4zudQQY/XgYaijyyo22ra2c=";
          allowedIPs = [ "10.100.0.2/32" ];
        }
        { # habilog, FIXME: not connecting when nat enabled
          publicKey = "OeCRZ1VuatQ1rGO7lw1S06so/3dSZfXRTKBx3S61kEY=";
          allowedIPs = [ "10.100.0.3/32" ];
        }
      ];
    };
  };

  sops.secrets.wg-private-key = {};
  sops.defaultSopsFile = ../secrets/au01/secrets.yaml;

}
