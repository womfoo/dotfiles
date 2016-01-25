# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let

  eremit-legacy = pkgs.callPackage /home/kranium/darcs/nix-eremit-legacy/default.nix { };

  gikosnet = pkgs.stdenv.mkDerivation {
    name = "gikosnet";
    src = ./.;
    installPhase = ''
      mkdir $out
      ln -s ${gikosnet-landing} $out/index.html
    '';
  };

  gikosnet-landing = pkgs.writeText "index.html" ''
    <html>
      <body>Move along citizen, nothing to see here!</body>
    </html>
    '';

  ralletacom = pkgs.stdenv.mkDerivation {
    name = "ralletacom";
    src = /home/kranium/home/k.ralleta.com;
    installPhase = ''
      mkdir $out
      ln -s * $out/
    '';
  };

  #namecheap domains with ssl
  gikosnet-aliases = [
    "confluence.gikos.net"
    "darcsden.gikos.net"
    "darcsit.gikos.net"
    "gitit.gikos.net"
    "hydra.gikos.net"
    "jira.gikos.net"
    "keycloak.gikos.net"
    "silverspark.gikos.net"
    "syncserver.gikos.net"
  ];

  nonssl-domains = [
    "muinark.com" #godaddy
    "kranium.net" #namecheap
    #"ralleta.com" "k.ralleta.com" "kaye.ralleta.com" #namecheap - commented as they are defined below
    "zweldo.com"  #google apps (via enom)
    #"kranium.gikos.net" #namecheap
  ];

  acd_cli = with pkgs.python33Packages; buildPythonPackage rec {

    name = pname + "-" + version;
    pname = "acd_cli";
    version = "0.3.1";

    disabled = !isPy33;
    doCheck = !isPy33;

    src = pkgs.fetchFromGitHub {
      owner = "yadayada";
      repo = pname;
      rev = version;
      sha256 = "1ywimbisgb5g7xl9nrfwcm7dv3j8fsrjfp7bxb3l58zbsrzj6z2s";
    };

    propagatedBuildInputs =  [ appdirs colorama dateutil requests2 requests_toolbelt sqlalchemy9 ];

    makeWrapperArgs = [ "--prefix LIBFUSE_PATH : ${pkgs.fuse}/lib/libfuse.so" ];

  };

in

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda";

  networking.hostName = "dixsept"; # Define your hostname.
  networking.interfaces.eth0 = { ipAddress = "195.154.164.244"; prefixLength = 24; };
  networking.defaultGateway  = "195.154.164.0";
  networking.nameservers     = [ "62.210.16.6" "62.210.16.7" ];
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  networking.firewall.allowedTCPPorts = [
    22
    80
    443
    3000
    4443
    8081
    8140
    8142
    22000
    61613
  ];
  networking.firewall.allowedUDPPorts = [
    21027
  ];
  networking.firewall.allowedUDPPortRanges = [
    { from = 60000; to = 61000; }
  ];
  networking.firewall.allowPing = true;

  # Select internationalisation properties.
  # i18n = {
  #   consoleFont = "Lat2-Terminus16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };

  # Set your time zone.
  # time.timeZone = "Europe/Amsterdam";

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  # environment.systemPackages = with pkgs; [
  #   wget
  # ];

  environment.systemPackages = with pkgs; [
    eremit-legacy
    mosh
    iotop
    htop
    nethogs
    (vsftpd.override { sslEnable = true; })
    acd_cli
  ];

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  services.fail2ban.enable = true;
  services.fail2ban.jails.ssh-iptables = "enabled = true";

  security.sudo.wheelNeedsPassword = false;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable the X11 windowing system.
  # services.xserver.enable = true;
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.kdm.enable = true;
  # services.xserver.desktopManager.kde4.enable = true;

  users.extraUsers.kranium =
    { createHome      = true;
      home            = "/home/kranium";
      description     = "your name";
      extraGroups     = [ "vboxusers" "wheel" ];
      useDefaultShell = true;
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCv8Rdf8gqewljlONxIU/NoI+aQhA0UNQbAsif0gKqGLPG2QrZgPktgG3r0Fn6cKtuhy7iExfWmUafJU73Od/hj8DK6uxicHEXh5pv6DZc2DEwyC5orJHQOZLblo96u2xsBkVx/++Nq/2vW1aMN0Wg8/Vgal1fBcfJAT9XAFmiKXLZxIvxWWw0PZYil4QJtlVGwebXm1trPr7H9hV8l+Lse8Z/Xt38DzQJI7yV5m6ENxPL/xCFsMMgb27c+Xf6gJPq2DIcUOJiP7fOcHXWN2W4/+ApUH5adMhJ8Y8mT4CGcLqNhcHKSFzPaUQpfQ0vi3QJez1LYoHANu6Iy6q7HoIab kranium@silverspark"
      ];
    };

  users.extraUsers.kayeralleta =
    { createHome      = true;
      home            = "/home/kayeralleta";
      useDefaultShell = true;
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCv8Rdf8gqewljlONxIU/NoI+aQhA0UNQbAsif0gKqGLPG2QrZgPktgG3r0Fn6cKtuhy7iExfWmUafJU73Od/hj8DK6uxicHEXh5pv6DZc2DEwyC5orJHQOZLblo96u2xsBkVx/++Nq/2vW1aMN0Wg8/Vgal1fBcfJAT9XAFmiKXLZxIvxWWw0PZYil4QJtlVGwebXm1trPr7H9hV8l+Lse8Z/Xt38DzQJI7yV5m6ENxPL/xCFsMMgb27c+Xf6gJPq2DIcUOJiP7fOcHXWN2W4/+ApUH5adMhJ8Y8mT4CGcLqNhcHKSFzPaUQpfQ0vi3QJez1LYoHANu6Iy6q7HoIab kranium@silverspark"
	"ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDS3glS9aKmAy76UoiMGUNL3C5FUnO8hZkmc/JPcaxET2kCQRzzjasK0YLJo0SQFC/+KRbe3RaSRbsOSUDynd/xslxJITReUwGp/EQdi0hpaBR/gwPQFiqvhn6OMGUHqVmATrvwWANgj6oEeGKmlzIInfxrBhVdENRpAdkiwSmWDJnSc8asV4Wt84XC0a53ePuzrCDi0NYhv2X2syHMYwm1xeCPNaUyNk8ztSlu3+uShxh1mlyYKR+vNDwPqImCwpHyFGUrTW4UpTDvD1Hw3vzenk8Ij32aPDHZrsXq1+79BLFPBvNtZhdmxuyv1WSqj/nZqRvyhvFVTrgqc1hNFfph armoire@Kayes-MacBook.local"
      ];
    };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  # users.extraUsers.guest = {
  #   isNormalUser = true;
  #   uid = 1000;
  # };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.03";

  services.syncthing.enable = true;
  services.syncthing.user = "kranium";
  services.syncthing.dataDir = "/data/syncthing";

  systemd.services.eremit-legacy = {
    description   = "eremit-legacy every 10 minutes";
    serviceConfig = {
      ExecStart = "${eremit-legacy}/bin/eremit /home/kranium/eremit-add03.txt";
    };
    startAt = "*:0/10";
    wantedBy = [ "default.target" ];
  };

  services.httpd = {
    adminAddr = "admin@gikos.net";

    sslServerCert = "/var/keys/gikos_net.cert";
    sslServerChain = "/var/keys/gikos_net.chain";
    sslServerKey = "/var/keys/gikos_net.key";

    enable = true;

    enablePHP = true;
    phpOptions = ''
      upload_max_filesize = 64M
      post_max_size = 64M
    '';

    virtualHosts = [
      { hostName = "gikos.net";
        serverAliases = gikosnet-aliases ++ ["www.gikos.net"];
        globalRedirect = "https://gikos.net";
      }
      { hostName = "ralleta.com";
        documentRoot = "/home/kayeralleta/wordpress";
        extraConfig = ''
          <Directory />
             DirectoryIndex index.php
             Allow from *
             Options FollowSymLinks
             AllowOverride All
             <IfModule mod_rewrite.c>
               RewriteEngine On
               RewriteBase /
               RewriteRule ^index\.php$ - [L]
               # add a trailing slash to /wp-admin
               RewriteRule ^wp-admin$ wp-admin/ [R=301,L]
               RewriteCond %{REQUEST_FILENAME} -f [OR]
               RewriteCond %{REQUEST_FILENAME} -d
               RewriteRule ^ - [L]
               RewriteRule ^(wp-(content|admin|includes).*) $1 [L]
               RewriteRule ^(.*\.php)$ $1 [L]
               RewriteRule . index.php [L]
             </IfModule>
          </Directory>
        '';
      }
      { hostName = "kaye.ralleta.com";
        serverAliases = ["k.ralleta.com" "www.ralleta.com"];
        globalRedirect = "http://ralleta.com";
      }
      #catchall for all other domains without any sites
      { hostName = "kranium.gikos.net";
        serverAliases = nonssl-domains;
        documentRoot = "${pkgs.apacheHttpd}/htdocs";
      }
      { hostName = "gikos.net";
        serverAliases = gikosnet-aliases;
        enableSSL = true;
        port = 443;
        documentRoot = "${gikosnet}";
      }
    ];
    extraConfig = ''
      SSLProtocol all -SSLv3 -SSLv2
      SSLHonorCipherOrder On
      SSLCipherSuite ECDH@STRENGTH:DH@STRENGTH:HIGH:!RC4:!MD5:!3DES:!DES:!aNULL:!eNULL
      Header set Strict-Transport-Security "max-age=15768000"
    '';
  };

  services.mysql = {
    enable = true;
    pidDir = "/var/run/mysql";
    package = pkgs.mysql;
  };

  services.vsftpd = {
    enable = true;
    userlistDeny = false;
    localUsers = true;
    userlist = ["kayeralleta" ];
    writeEnable = true;
  };

  virtualisation.virtualbox.host.enable = true;

}
