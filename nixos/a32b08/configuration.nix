# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, ... }:
let
  hostName = "a32b08";
in
{

  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ../shared/builder.nix
    # ../shared/gikos-telegraf.nix
    ../shared/gikos-kranium.nix
  ];

  boot.loader.generic-extlinux-compatible.enable = true;
  boot.loader.grub.enable = false;
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernelPatches = lib.singleton {
      name = "mainline 5.13 disable ks8851";
      patch = null;
      extraConfig = ''
        KS8851 n
      '';
    };

  # tmpfs makes builds faster than sd
  /*
  fileSystems."/tmp" =
    { device = "habilog.gikos.net:/armorydata/${hostName}";
      fsType = "nfs";
      options = ["auto" "nofail" "soft"];
    };
  */


  powerManagement.powerDownCommands = ''
    logger 'Trying to stop harddisk'
    ${pkgs.hdparm}/sbin/hdparm -y /dev/sda
    logger 'Trying to wait for hdd'
    ${pkgs.coreutils}/bin/sleep 5
    logger 'Ending wait for hdd'
  '';

  networking.hostName = hostName;

  environment.systemPackages = with pkgs; [
 
    w3m-nographics # needed for the manual anyway
    testdisk # useful for repairing boot problems
    ms-sys # for writing Microsoft boot sectors / MBRs
    efibootmgr
    efivar
    parted
    gptfdisk
    ddrescue
    ccrypt
    cryptsetup # needed for dm-crypt volumes
    mkpasswd # for generating password files
    ntp
# Some text editors.
    vim

# Some networking tools.
    fuse
    fuse3
    sshfs-fuse
    rsync
    socat
    screen

# Hardware-related tools.
    sdparm
    hdparm
    smartmontools # for diagnosing hard disks
    pciutils
    usbutils

# Tools to create / manipulate filesystems.
    ntfsprogs # for resizing NTFS partitions
    dosfstools
    xfsprogs.bin
    jfsutils
    f2fs-tools

# Some compression/archiver tools.
    unzip
    zip


    darcs
    dnsutils
    emacs-nox
    git
    htop
    i2c-tools
    iotop
    iperf
    iptraf-ng
    kubernetes
    mc
    mmc-utils
    ncdu
    nethogs
    nix-top
    speedtest-cli
    tcpdump
    wget
  ];

  services.openssh.enable = true;

  nixpkgs.overlays = [
    (import ../shared/overlay-armv7l.nix) 
  ];

/*
  services.xserver = {
    enable = true;
    desktopManager.xterm.enable = false;
    displayManager.defaultSession = "none+xmonad";
    displayManager.sddm.enable = true;
    windowManager.xmonad.enable = true;
    windowManager.xmonad.enableContribAndExtras = true;
  };
*/

  system.stateVersion = "21.11"; # Did you read the comment?

  systemd.services.park-sda =
    { description = "Try to park harddisk";


      after = [ "umount.target" ];
      wantedBy = [ "shutdown.target" ];

      serviceConfig = {
        Type = "oneshot";
        ExecStart = pkgs.writeShellScript "park-sda-script" ''
           
          ${pkgs.hdparm}/sbin/hdparm -y /dev/sda;
          ${pkgs.coreutils}/bin/sleep 5;
        '';




      };
    };


  time.timeZone = "Australia/Sydney";





}

