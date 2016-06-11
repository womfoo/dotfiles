# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  ikvm-launch = pkgs.callPackage /home/kranium/git/github.com/womfoo/nix-launch-ikvm { };
  ldapseed = pkgs.callPackage /home/kranium/darcs/nix-ldapseed/default.nix { };
  idpmetadata = pkgs.fetchurl {
    url = "https://kranium.oktapreview.com/app/exk5sig0ciaHGuguQ0h7/sso/saml/metadata";
    sha256 = "1b9xi5p6nv2mb00wl1961cm909vablxxl41fndpfh6agj6r7xmrg";
  };
  spfiles = pkgs.stdenv.mkDerivation {
    name = "localhost-spfiles";
    src = ./.;
    buildInputs = [ pkgs.openssl ];
    buildPhase = ''
      ${pkgs.apacheHttpdPackages.mod_auth_mellon}/bin/mellon_create_metadata.sh localhost http://localhost/mellon
    '';
    installPhase = ''
      mkdir -p $out/private
      mkdir -p $out/public
      cp localhost.key $out/private
      cp localhost.cert $out/public
      cp localhost.xml $out/public
    '';
  };
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the gummiboot efi boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.timeout = 4;
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
    python33Packages.acd_cli
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
    cryptsetup
    darcs
    deadbeef
    debootstrap
    dejavu_fonts
    dillo
    dmenu
    dropbox
    duc
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
    gparted
    #(haskellPackages.ghcWithPackages (self : with haskellPackages; with pkgs.haskell.lib; [
    (haskell.packages.ghc7103.ghcWithPackages (self : with haskell.packages.ghc7103; with pkgs.haskell.lib; [
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
    parted
    patchelf
    pavucontrol
    pciutils                    # setpci
    pdftk
    pgadmin
    pidgin
    pipes                       # screensaver
    pkgconfig
    pmtools                     # acpidump
    poppler_utils
    postgresql #just for the psql command
    psmisc
    pv
    qpdf
    remmina                     # rdp
    rsync
    ruby_2_1
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
    xscreensaver
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
  #services.mbpfan.lowTemp = 55;   # try ranges 55-63, default is 63
  #services.mbpfan.highTemp = 58;  # try ranges 58-66, default is 66
  #services.mbpfan.maxTemp = 78;   # do not set it > 90, default is 86
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

  nixpkgs.config = {
    allowUnfree = true;
    firefox = {
      enableAdobeFlash = true;
      enableGoogleTalkPlugin = true; #nonfree
      icedtea = true;
    };
  };

  security.sudo.wheelNeedsPassword = false;

  services.httpd = {
    enable = true;
    adminAddr = "admin@localhost";
    enableMellon = true;
    virtualHosts = [
      { hostName = "localhost";
        documentRoot = "${spfiles}";
        extraConfig = ''
        <Location />
            MellonEnable "info"
            MellonSPPrivateKeyFile ${spfiles}/private/localhost.key
            MellonSPCertFile ${spfiles}/public/localhost.cert
            MellonSpMetadataFile ${spfiles}/public/localhost.xml
            MellonIdPMetadataFile ${idpmetadata}
            MellonEndpointPath "/mellon"
        </Location>
        <Location /private>
            MellonEnable "auth"
        </Location>
        '';
       }
    ];
  };

}
