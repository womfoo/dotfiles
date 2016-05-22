# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  secrets = import ./secrets.nix;
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./nixos-in-place.nix
    ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # Define on which hard drive you want to install Grub.
  # boot.loader.grub.device = "/dev/sda";

  networking.hostName = "zehn"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.firewall.extraCommands = ''
    iptables -I nixos-fw 2 -p udp -s ${secrets.home.publicip} --dport 53 -j nixos-fw-accept
    iptables -I nixos-fw 2 -p tcp -s ${secrets.home.publicip} --dport 80 -j nixos-fw-accept
    iptables -I nixos-fw 2 -p tcp -s ${secrets.home.publicip} --dport 443 -j nixos-fw-accept
  '';

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

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

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

  # Define a user account. Don't forget to set a password with ‘passwd’.
  # users.extraUsers.guest = {
  #   isNormalUser = true;
  #   uid = 1000;
  # };
  users.extraUsers.kranium = {
    createHome      = true;
    home            = "/home/kranium";
    description     = "your name";
    extraGroups     = [ "wheel" ];
    useDefaultShell = true;
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCv8Rdf8gqewljlONxIU/NoI+aQhA0UNQbAsif0gKqGLPG2QrZgPktgG3r0Fn6cKtuhy7iExfWmUafJU73Od/hj8DK6uxicHEXh5pv6DZc2DEwyC5orJHQOZLblo96u2xsBkVx/++Nq/2vW1aMN0Wg8/Vgal1fBcfJAT9XAFmiKXLZxIvxWWw0PZYil4QJtlVGwebXm1trPr7H9hV8l+Lse8Z/Xt38DzQJI7yV5m6ENxPL/xCFsMMgb27c+Xf6gJPq2DIcUOJiP7fOcHXWN2W4/+ApUH5adMhJ8Y8mT4CGcLqNhcHKSFzPaUQpfQ0vi3QJez1LYoHANu6Iy6q7HoIab kranium@silverspark"
    ];
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.03";

  services.sniproxy.enable = true;
  services.sniproxy.config = ''
    error_log {
        filename /var/log/sniproxy/error.log
        priority notice
    }
    
    access_log {
        filename /var/log/sniproxy/access.log
    }
    
    listen 80 {
        proto http
        bad_requests log
    }
    
    listen 443 {
        proto tls
        bad_requests log
    }
    
    table {
        hulu\.com *
        netflix(\.com|\.net) *
        go\.com *
        (nbc|nbcuni)\.com *
        (mtv|mtvnservices)\.com *
        theplatform\.com *
        a248\.e\.akamai\.net *
        video\.dl\.playstation\.net *
        pandora\.com *
        crackle\.com *
        vevo\.com *
        crunchyroll\.com *
        dramafever\.com *
        optimizely\.com *
        uplynk\.com *
        maxmind\.com *
        ipinfo\.io *
        brightcove\.com *
        iheart\.com *
        logotv\.com *
        pbs\.org *
        warnerbros\.com *
        cwtv\.com *
        smithsonianchannel\.com *
        southpark\.cc\.com *
        spike\.com *
        vh1\.com *
        fxnetworks\.com *
        ipad-streaming\.cbs\.com *
        tve_nbc-vh\.akamaihd\.net *
        sni-vh\.akamaihd\.net *
        unicornmedia\.com *
        hbogo\.com *
        hbonow\.com *
        amazon\.com *
        dramafevertoken-a\.akamaihd\.net *
        disney\.com *
        disneyjunior\.com *
        nbcsports\.com *
        tvenbcsn-i\.akamaihd\.net *
        nbcstreameast\.nbcsports\.com *
        tvegolf-i\.akamaihd\.net *
        sprtott1-i\.akamaihd\.net *
        sprtott2-i\.akamaihd\.net *
        sprtott3-i\.akamaihd\.net *
        sprtott4-i\.akamaihd\.net *
        sprtott5-i\.akamaihd\.net *
        sprtott6-i\.akamaihd\.net *
        sprtott7-i\.akamaihd\.net *
        sprtott8-i\.akamaihd\.net *
        sprtott9-i\.akamaihd\.net *
        sprtott10-i\.akamaihd\.net *
        sprtott11-i\.akamaihd\.net *
        sprtott12-i\.akamaihd\.net *
        sprtott13-i\.akamaihd\.net *
        sprtott14-i\.akamaihd\.net *
        sprtott15-i\.akamaihd\.net *
        ftve7300-i\.akamaihd\.net *
        nflsvgatlgame3-i\.akamaihd\.net *
        nflioslivesvg2-i\.akamaihd\.net *
        foxsportshdhls-lh\.akamaihd\.net *
        beinsportgeolivefs\.fplive\.net *
        beinsportsconnect\.tv *
        beinsportsconnect\.net *
        amctv\.com *
    }
 '';

  services.dnsmasq.enable = true;
  services.dnsmasq.resolveLocalQueries = false;
  services.dnsmasq.servers = ["8.8.8.8" "8.8.4.4"];
  services.dnsmasq.extraConfig = ''
    server=/med3.hbonow.com/8.8.8.8
    address=/hulu.com/${secrets.nixflix.publicip}
    address=/netflix.com/${secrets.nixflix.publicip}
    address=/netflix.net/${secrets.nixflix.publicip}
    address=/mtv.com/${secrets.nixflix.publicip}
    address=/mtvnservices.com/${secrets.nixflix.publicip}
    address=/optimizely.com/${secrets.nixflix.publicip}
    address=/go.com/${secrets.nixflix.publicip}
    address=/a248.e.akamai.net/${secrets.nixflix.publicip}
    address=/video.dl.playstation.net/${secrets.nixflix.publicip}
    address=/pandora.com/${secrets.nixflix.publicip}
    address=/uplynk.com/${secrets.nixflix.publicip}
    address=/crackle.com/${secrets.nixflix.publicip}
    address=/maxmind.com/${secrets.nixflix.publicip}
    address=/theplatform.com/${secrets.nixflix.publicip}
    address=/dramafever.com/${secrets.nixflix.publicip}
    address=/crunchyroll.com/${secrets.nixflix.publicip}
    address=/iheart.com/${secrets.nixflix.publicip}
    address=/brightcove.com/${secrets.nixflix.publicip}
    address=/logotv.com/${secrets.nixflix.publicip}
    address=/nbcuni.com/${secrets.nixflix.publicip}
    address=/nbc.com/${secrets.nixflix.publicip}
    address=/pbs.org/${secrets.nixflix.publicip}
    address=/cwtv.com/${secrets.nixflix.publicip}
    address=/warnerbros.com/${secrets.nixflix.publicip}
    address=/vevo.com/${secrets.nixflix.publicip}
    address=/smithsonianchannel.com/${secrets.nixflix.publicip}
    address=/vh1.com/${secrets.nixflix.publicip}
    address=/spike.com/${secrets.nixflix.publicip}
    address=/southpark.cc.com/${secrets.nixflix.publicip}
    address=/tve_nbc-vh.akamaihd.net/${secrets.nixflix.publicip}
    address=/sni-vh.akamaihd.net/${secrets.nixflix.publicip}
    address=/ipad-streaming.cbs.com/${secrets.nixflix.publicip}
    address=/ipinfo.io/${secrets.nixflix.publicip}
    address=/unicornmedia.com/${secrets.nixflix.publicip}
    address=/hbogo.com/${secrets.nixflix.publicip}
    address=/hbonow.com/${secrets.nixflix.publicip}
    address=/amazon.com/${secrets.nixflix.publicip}
    address=/dramafevertoken-a.akamaihd.net/${secrets.nixflix.publicip}
    address=/ulive.com/${secrets.nixflix.publicip}
    address=/disney.com/${secrets.nixflix.publicip}
    address=/disneyjunior.com/${secrets.nixflix.publicip}
    address=/nbcsports.com/${secrets.nixflix.publicip}
    address=/tvenbcsn-i.akamaihd.net/${secrets.nixflix.publicip}
    address=/nbcstreameast.nbcsports.com/${secrets.nixflix.publicip}
    address=/tvegolf-i.akamaihd.net/${secrets.nixflix.publicip}
    address=/sprtott1-i.akamaihd.net/${secrets.nixflix.publicip}
    address=/sprtott2-i.akamaihd.net/${secrets.nixflix.publicip}
    address=/sprtott3-i.akamaihd.net/${secrets.nixflix.publicip}
    address=/sprtott4-i.akamaihd.net/${secrets.nixflix.publicip}
    address=/sprtott5-i.akamaihd.net/${secrets.nixflix.publicip}
    address=/sprtott6-i.akamaihd.net/${secrets.nixflix.publicip}
    address=/sprtott7-i.akamaihd.net/${secrets.nixflix.publicip}
    address=/sprtott8-i.akamaihd.net/${secrets.nixflix.publicip}
    address=/sprtott9-i.akamaihd.net/${secrets.nixflix.publicip}
    address=/sprtott10-i.akamaihd.net/${secrets.nixflix.publicip}
    address=/sprtott11-i.akamaihd.net/${secrets.nixflix.publicip}
    address=/sprtott12-i.akamaihd.net/${secrets.nixflix.publicip}
    address=/sprtott13-i.akamaihd.net/${secrets.nixflix.publicip}
    address=/sprtott14-i.akamaihd.net/${secrets.nixflix.publicip}
    address=/sprtott15-i.akamaihd.net/${secrets.nixflix.publicip}
    address=/ftve7300-i.akamaihd.net/${secrets.nixflix.publicip}
    address=/nflsvgatlgame3-i.akamaihd.net/${secrets.nixflix.publicip}
    address=/nflioslivesvg2-i.akamaihd.net/${secrets.nixflix.publicip}
    address=/foxsportshdhls-lh.akamaihd.net/${secrets.nixflix.publicip}
    address=/beinsportgeolivefs.fplive.net/${secrets.nixflix.publicip}
    address=/beinsportsconnect.tv/${secrets.nixflix.publicip}
    address=/beinsportsconnect.net/${secrets.nixflix.publicip}
    address=/amctv.com/${secrets.nixflix.publicip}
  '';
}
