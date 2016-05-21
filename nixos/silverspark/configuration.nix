# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  ikvm-launch = pkgs.callPackage /home/kranium/git/github.com/womfoo/nix-launch-ikvm { };
  ldapseed = pkgs.callPackage /home/kranium/darcs/nix-ldapseed/default.nix { };
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the gummiboot efi boot loader.
  boot.loader.gummiboot.enable = true;
  boot.loader.gummiboot.timeout = 4;
  boot.loader.efi.canTouchEfiVariables = true;

  networking = {
    hostName = "silverspark";
    networkmanager.enable = true;
    networkmanager.insertNameservers = [ "8.8.8.8" "8.8.4.4" ];
    firewall.allowedTCPPorts = [
      22
    ];
  };

  # Select internationalisation properties.
  # i18n = {
  #   consoleFont = "lat9w-16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };

  # List packages installed in system profile. To search by name, run:
  # -env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    # work
    bluejeans
    hipchat
    skype
    # local
    ikvm-launch
    ldapseed
    # mine
    abcde
    acpi
    ant
    arandr
    aria2
    audacity
    augeas
    baobab
    bind                        # nslookup
    binutils                    # ld, ar
    bitcoin
    btrfs-progs
    bundix
    bundler
    chromium
    cifs_utils
    compton
    cool-retro-term
    cpuminer-multi
    cryptsetup
    darcs
    deadbeef
    debootstrap
    dejavu_fonts
    dillo
    dmenu
    dropbox
    (with emacsPackagesNg; emacsWithPackages [
      ace-jump-mode
      color-theme
      haskell-mode
      magit
    ])
    encfs
    exfat
    exfat-utils
    facter
    fbreader
    file
    firefox
    flac
    fuse_exfat
    geoip
    ghostscript
    gimp
    gitFull
    glxinfo
    gnome3.cheese
    gnome3.eog
    gnome3.evince
    gnome3.nautilus
    gnucash
    gnupg1compat
    go-mtpfs                    # jmtpfs and mtpfs fails on my xiaomi
    gpa
    (haskellPackages.ghcWithPackages (self : with haskellPackages; with pkgs.haskell.lib; [
      ShellCheck
      alex
      cabal-install
      cabal2nix
      ghc-mod
      hledger
      wreq
      xmobar
    ]))
    htop
    ifuse
    imagemagick
    inetutils
    inkscape
    inotify-tools
    iotop
    jq
    kazam
    kde4.digikam
    kde4.gwenview
    kde4.kdenlive
    kde4.kdiff3
    kde4.ktorrent
    keepassx
    keybase
    kpcli
    libreoffice
    libxslt
    logstash
    lsof
    lxc
    mc
    meld
    mercurialFull
    mosh
    mplayer
    (mtr.override { withGtk = true; })
    ncdu
    nethogs
    networkmanagerapplet
    ntfs3g
    oathToolkit
    openjdk
    openssl
    pandoc
    patchelf
    pavucontrol
    pciutils                    # setpci
    pdftk
    pgadmin
    pidgin
    pkgconfig
    pmtools                     # acpidump
    poppler_utils
    psmisc
    pv
    qpdf
    remmina                     # rdp
    rsync
    ruby
    rxvt_unicode
    simplescreenrecorder
    smartmontools
    speedtest-cli
    sqlite
    squashfsTools
    sshfsFuse
    subversion
    tcpdump
    tesseract
    thunderbird
    torbrowser
    trayer
    tree
    unzip
    usbutils                    # lsusb
    vagrant
    vlc
    vnstat
    wget
    which
    wireshark
    xawtv
    xcalib                      # calibrate colors
    xfontsel
    xlibs.xkill
    xlibs.xwd
    xlockmore
    xmlsec
    xorg.xdpyinfo
    xorg.xlsfonts               # font for xosd
    xorg.xwininfo
    xosd
    xsane
    xzgv
    youtubeDL
  ];

  # List services that you want to enable:
  services.acpid.enable = true;
  services.upower.enable = true;

  services.mbpfan.enable = true;
  services.mbpfan.lowTemp = 55;   # try ranges 55-63, default is 63
  services.mbpfan.highTemp = 58;  # try ranges 58-66, default is 66
  services.mbpfan.maxTemp = 78;   # do not set it > 90, default is 86
  #services.mbpfan.verbose = true;

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
  services.xserver.synaptics.fingersMap = [1 3 2];
  services.xserver.synaptics.twoFingerScroll = true;

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.kdm.enable = true;
  # services.xserver.desktopManager.kde4.enable = true;
  services.xserver.desktopManager.xterm.enable = false;
  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.videoDrivers = [ "nvidia" ]; #non-free
  services.xserver.windowManager.xmonad.enable = true;
  services.xserver.windowManager.xmonad.enableContribAndExtras = true;
  services.xserver.windowManager.default = "xmonad";

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.kranium = {
     name = "kranium";
     extraGroups = [ "wheel" "networkmanager" "audio" "docker" "vboxusers" "video"];
     group = "users";
     uid = 2000;
     createHome = true;
     home = "/home/kranium";
     shell = "/run/current-system/sw/bin/bash";
  };
  users.extraGroups = { networkmanager = { } ; kranium = { gid = 2000; } ; } ;

  hardware.facetimehd.enable = true;
  hardware.pulseaudio.enable = true;

  time.timeZone = "Asia/Kuala_Lumpur";

  virtualisation.docker.enable = true;
  virtualisation.virtualbox.host.enable = true;

}
