# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the gummiboot efi boot loader.
  boot.loader.gummiboot.enable = true;
  boot.loader.gummiboot.timeout = 4;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "silverpen";
  # networking.wireless.enable = true;  # Enables wireless.
  networking.networkmanager.enable = true;

  # Select internationalisation properties.
  # i18n = {
  #   consoleFont = "lat9w-16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };

  # List packages installed in system profile. To search by name, run:
  # -env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    arandr
    audacity
    baobab
    bind                        # nslookup
    bitcoin
    chromium
    darcs
    dmenu
    dropbox
    emacs
    emacs24Packages.haskellMode
    file
    firefox
    gimp
    gimp
    git
    gnucash
    gparted
    haskellPackages.xmobar
    htop
    iotop
    kde4.ktorrent
    keepassx
    libreoffice
    logstash
    mc
    meld
    networkmanagerapplet
    pavucontrol
    pgadmin
    pidgin
    pmtools                     # acpidump
    postgresql
    psmisc
    pulseaudio
    rsync
    rxvt_unicode
    subversion
    trayer
    unzip
    vlc
    wget
    wireshark
  ];

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";
  services.xserver.synaptics.enable = true;
  services.xserver.synaptics.palmDetect = true;
  services.xserver.synaptics.twoFingerScroll = true;

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.kdm.enable = true;
  # services.xserver.desktopManager.kde4.enable = true;
  services.xserver.windowManager.xmonad.enable = true;
  services.xserver.windowManager.xmonad.enableContribAndExtras = true;
  services.xserver.windowManager.default = "xmonad";

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.kranium = {
     name = "kranium";
     extraGroups = [ "wheel" "networkmanager" "audio"];
     group = "users";
     uid = 2000;
     createHome = true;
     home = "/home/kranium";
     shell = "/run/current-system/sw/bin/bash";
  };
  users.extraGroups = { networkmanager = { } ; kranium = { gid = 2000; } ; } ;

  hardware.pulseaudio.enable = true;

  time.hardwareClockInLocalTime = true;
  time.timeZone = "Asia/Kuala_Lumpur";

}
