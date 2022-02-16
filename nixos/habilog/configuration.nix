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

  # OpenSSH is forced to have an empty `wantedBy` on the installer system[1], this won't allow it
  # to be automatically started. Override it with the normal value.
  # [1] https://github.com/NixOS/nixpkgs/blob/9e5aa25/nixos/modules/profiles/installation-device.nix#L76
  #systemd.services.sshd.wantedBy = lib.mkOverride 40 [ "multi-user.target" ];

  # Enable OpenSSH out of the box.
  services.sshd.enable = true;

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
  ];
  networking.hostId = "6ea8191e";

  environment.systemPackages = with pkgs; [
    darcs
    dnsutils
    parted
    hub
    nix-top
    youtube-dl
    ntfs3g
    python3Packages.pip # test if octoprint works with this
    ncdu
    mc
    screen
    emacs-nox
    dmenu
    vim
    git
    darcs
    iotop
    nethogs
    tmux
    tcpdump
    speedtest-cli
    #arandr
    #st
    wget
    #ffmpeg
    ifuse
    iw
    bitcoin
    smartmontools
    nmon
  ];

  time.timeZone = "Australia/Sydney";

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

}
