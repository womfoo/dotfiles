# mostly based on https://github.com/angerman/nixos-docker-sd-image-builder
{ config, options, pkgs, lib, ... }:
{
  imports = [
    # <nixpkgs/nixos/modules/installer/cd-dvd/sd-image.nix>
    # <nixpkgs/nixos/modules/installer/cd-dvd/sd-image-aarch64.nix>
    ../shared/common.nix
    ../shared/timescaledb.nix
    ./modules/rtmp.nix
    ./helios64/default.nix
    ../shared/gikos-kranium.nix
    ../shared/gikos-net-bind.nix
    ../shared/router.nix
    ../shared/secrets.nix
  ];

  services.nginx = {
    enable = true;
  };

  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;
  boot.consoleLogLevel = lib.mkDefault 7;
  boot.initrd.kernelModules = [ "phy_rockchip_pcie" "md_mod" "dm_mod" "ahci" "usb_storage" "pci" "pcie_rockchip_host" "md"  #FIXME: find the bare minimum
    "ledtrig_netdev" /* "ledtrig-usbport" */ # FIXME: ledtrig does not blink
  ];

  fileSystems = {
    "/" = {
      device = "/dev/md/habilog";
      fsType = "ext4";
    };
    "/boot" = {
      device = "/dev/disk/by-label/NIXOS_BOOT";
      fsType = "ext4";
    };
    "/var/db/influxdb" =
    { device = "habilogpool/influxdb";
      fsType = "zfs";
    };
    "/var/spool/mail" =
    { device = "habilogpool/mail";
      fsType = "zfs";
    };
    "/armorydata" =
    { device = "habilogpool/armorydata";
      fsType = "zfs";
    };
    "/dashcam" =
    { device = "habilogpool/dashcam";
      fsType = "zfs";
    };
    "/2021refuge" =
    { device = "habilogpool/2021refuge";
      fsType = "zfs";
    };
    "/legacydata" =
    { device = "habilogpool/legacydata";
      fsType = "zfs";
    };
    "/home/kranium/.bitcoin" =
    { device = "habilogpool/btcchain";
      fsType = "zfs";
    };
    "/phonebackup" =
    { device = "habilogpool/phonebackup";
      fsType = "zfs";
    };
  };
  swapDevices = [
    { device = "/dev/md/swap"; }
  ];

  programs.ssh.startAgent = true;

  # the installation media is also the installation target,
  # so we don't want to provide the installation configuration.nix.
  #installer.cloneConfig = false;

  boot.extraModulePackages = with config.boot.kernelPackages; [ rtl8814au ];
  boot.kernelModules = [ "cpufreq-conservative" ];

  # nfs makes this unstable
  services.nfs.server.enable = true;
  services.nfs.server.statdPort = 47000;
  services.nfs.server.mountdPort = 47001;
  services.nfs.server.lockdPort = 47002;
  services.nfs.server.exports    = ''
      /dashcam *(ro,insecure,no_root_squash,no_subtree_check)
      /armorydata 172.19.86.100/31(rw,no_root_squash,no_subtree_check)
      /legacydata 172.19.86.100/31(rw,no_root_squash,no_subtree_check)
      /nix/store 172.19.86.100/31(ro,no_root_squash,no_subtree_check)
      /armorydata/a32b1 172.19.86.201/32(rw,no_root_squash,no_subtree_check)
      /armorydata/a32b2 172.19.86.202/32(rw,no_root_squash,no_subtree_check)
      /armorydata/a32b3 172.19.86.203/32(rw,no_root_squash,no_subtree_check)
      /armorydata/a32b08 172.19.86.203/32(rw,no_root_squash,no_subtree_check)
      /phonebackup 172.19.86.100/24(rw,no_root_squash,no_subtree_check)
    '';
  networking.timeServers = [
    "0.au.pool.ntp.org"
    "1.au.pool.ntp.org"
    "2.au.pool.ntp.org"
    "3.au.pool.ntp.org"
  ];

  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = [
    22
    80
    143
    111 # nfs
    443 # doh
    1935 # rtp
    1936 # rtp website
    2049 # nfs
    3000 # tempfixme hydra
    config.services.nfs.server.statdPort
    config.services.nfs.server.mountdPort
    config.services.nfs.server.lockdPort
    5000 # octoprint
    8086 # influxdb
    9050 # can we move this to config.services.tor.settings.SOCKSPort?
  ];
  networking.firewall.allowedUDPPorts = [
    53
    111 # nfs
    2049 # nfs
    config.services.nfs.server.statdPort
    config.services.nfs.server.mountdPort
    config.services.nfs.server.lockdPort
    config.networking.wireguard.interfaces.wg0.listenPort
  ];
  networking.hostId = "6ea8191e";
  networking.hostName = "habilog";

  environment.systemPackages = with pkgs; [
    bitcoin
    darcs
    darcs
    dmenu
    dnsutils
    emacs-nox
    ffmpeg-full
    git
    hub
    ifuse
    iotop
    iw
    mc
    ncdu
    nethogs
    nix-top
    nmon
    ntfs3g
    parted
    python3Packages.pip # test if octoprint works with this
    screen
    smartmontools
    speedtest-cli
    tcpdump
    tmux
    vim
    wget
    youtube-dl
  ];

  security.acme.acceptTerms = true;
  security.acme.defaults.email = "kranium@gikos.net";

  services.usbmuxd.enable = true;

  security.sudo.wheelNeedsPassword = false;

  # FIXME: avoid hangs, apply RAM fix
  powerManagement.cpuFreqGovernor = "conservative";

  hardware.enableAllFirmware = true;
  hardware.pulseaudio.enable = true;
  hardware.firmware = [ pkgs.wireless-regdb ];

  boot.supportedFilesystems = [ "zfs" ];

  nixpkgs.config = {
    allowUnsupportedSystem = true;
    allowUnfree = true;
    allowBroken = true;
  };

  boot.initrd.mdadmConf = ''
    ARRAY /dev/md/habilog metadata=1.2 name=habilog:NIXOS_ROOT UUID=622fc7b7:7a5e10ae:7b70659f:24f1b2cb
    ARRAY /dev/md/swap metadata=1.2 name=habilog:NIXOS_SWAP UUID=28865355:1ebf9b51:8b4191ca:a9bf583a
  '';
  services.dovecot2.enable = true;
  services.dovecot2.extraConfig = ''
    service imap {
    vsz_limit = 512 M
    }
  '';

  services.roundcube.enable = true;
  services.roundcube.hostName = "rc.gikos.net";

  services.octoprint.enable = true;
  users.extraUsers.octoprint = {
    extraGroups = [ "dialout"];
  };

  nix.extraOptions = ''
    keep-outputs = true
  '';

  systemd.watchdog.device = "/dev/watchdog";

  networking.wireguard.interfaces = {
    wg0 = {
      ips = [ "10.100.0.3/24" ];
      listenPort = 51820; # to match firewall allowedUDPPorts (without this wg uses random port numbers)
      privateKeyFile = config.sops.secrets.wg-private-key.path;
      peers = [
        {
          publicKey = "ykVVI6YfSDarxLfDwwCrnA7KYNUD3lHyK0QkGFtXIgA=";
          allowedIPs = [ "10.100.0.0/24" ];
          endpoint = "149.28.180.243:51820";
          persistentKeepalive = 25;
        }
      ];
    };
  };

  sops.secrets.wg-private-key = {};
  sops.defaultSopsFile = ../secrets/habilog/secrets.yaml;

  zramSwap.enable = lib.mkDefault true;

}
