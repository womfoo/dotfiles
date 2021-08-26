# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:
let
  hostName = "a32b08";
in
{

  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./shared/builder.nix
    # ./shared/gikos-telegraf.nix
    ./shared/gikos-kranium.nix
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
  fileSystems."/tmp" =
    { device = "habilog.gikos.net:/armorydata/${config.sdImage.imageBaseName}";
      fsType = "nfs";
      options = ["auto" "nofail" "soft"];
    };


  networking.hostName = "${config.sdImage.imageBaseName}";

  environment.systemPackages = with pkgs; [
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

}

