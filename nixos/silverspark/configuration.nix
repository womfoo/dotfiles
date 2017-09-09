# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  mycv = pkgs.callPackage /home/kranium/git/bitbucket.org/womfoo/awesome-cv { };
  ikvm-launch = pkgs.callPackage /home/kranium/git/github.com/womfoo/nix-launch-ikvm { };
  ldapseed = pkgs.callPackage /home/kranium/darcs/nix-ldapseed/default.nix { };
  #hnix_loc = pkgs.callPackage /home/kranium/git/github.com/jwiegley/hnix/default.nix { };
  idpmetadata = pkgs.fetchurl {
    url = "https://kranium.oktapreview.com/app/exk5sig0ciaHGuguQ0h7/sso/saml/metadata";
    sha256 = "023bvc32dw4wcxn53b38rl7mbyb5bh5vl3dfhschjb100g61a979";
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
      cp ${mycv}/resume.pdf $out/private
    '';
  };
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  boot.kernelPackages = pkgs.linuxPackages_4_8;

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
      2049 # nfs
      4000 # nfs/statd
      4001 # nfs/lockd
      4002 # nfs/mountd
    ];
    firewall.allowedUDPPorts = [
      2049 # nfs
      4000 # nfs/statd
      4001 # nfs/lockd
      4002 # nfs/mountd
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
    # maintainted
    facter
    fnotifystat
    forkstat
    gpxsee
    powerstat
    smemstat
    xzgv
    # work
    #bluejeans
    hipchat
    skype
    slack
    # local
    ikvm-launch
    ldapseed
    # mine
    abcde
    python33Packages.acd_cli
    abiword
    acpi
    #ant
    arandr
    aria2
    audacity
    augeas
    awscli
    #baobab
    bind                        # nslookup
    binutils                    # ld, ar
    bitcoin
    bmon
    btrfs-progs
    #bundix
    #bundler
    calibre
    chromium
    cifs_utils
    compton
    conkeror
    cool-retro-term
    cpuminer-multi
    cryptsetup
    darcs
    #deadbeef
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
      graphviz-dot-mode
    ])
    elfutils
    encfs
    ec2_api_tools
    ec2_ami_tools
    exfat
    exfat-utils
    facter
    fbreader
    file
    #firefox
    #firefox-beta-bin
    flac
    fuse_exfat
    geoip
    ghostscript
    #gimp
    gitFull
    glxinfo
    gnome3.cheese
    #gnome3.eog
    #gnome3.evince              # not built in unstable-small <2016-11-05>
    #gnome3.nautilusn
    #gnucash                    # not built in unstable-small <2016-11-04>
    #gnumeric
    gnupg1compat
    go-mtpfs                    # jmtpfs and mtpfs fails on my xiaomi
    gnutls
    gpa
    gparted
    gptfdisk
    #gtkgnutella
    graphviz
    (haskellPackages.ghcWithPackages (self : with haskellPackages; with pkgs.haskell.lib; [
    #(haskell.packages.ghc7103.ghcWithPackages (self : with haskell.packages.ghc7103; with pkgs.haskell.lib; [
      #hsass
      #ShellCheck
      #alex
      cabal-install
      cabal2nix
      ghc-mod
      hledger
      #wreq
      xmobar
      #hnix
      #hnix_loc
      hGelf
      #gender
      #hakyll
      #hakyll-sass
      #aeson-pretty
      #stack
      #hails
    ]))
    htop
    ifuse
    imagemagick
    inetutils
    innoextract
    inkscape
    inotify-tools
    iotop
    iw                          # iw list
    jq
    kazam
    #kde4.digikam
    #kde4.gwenview
    #kde4.kdenlive
    kde4.kdiff3
    kde4.ktorrent
    keepassx
    keybase
    kpcli
    #libreoffice
    libva-full                  # vaapiVdpau should installt this but I need vainfo
    #libvdpau-va-gl              # vdpauinfo
    vdpauinfo                   # lol
    libxml2                     #xmllint
    libxslt
    logstash
    lsof
    #lxc
    mc                          # not built in unstable-small <2016-11-04>
    meld
    mercurialFull
    monero
    mosh
    #mplayer
    (mtr.override { withGtk = true; })
    ncdu
    nethogs
    networkmanagerapplet
    nix-prefetch-git
    nix-repl
    nox
    npm2nix
    ntfs3g
    oathToolkit
    openjdk
    openssl
    openttd
    pandoc
    parted
    patchelf
    pavucontrol
    pciutils                    # setpci
    pdftk
    pdfmod
    pgadmin
    p7zip
    #pidgin
    pipes                       # screensaver
    pkgconfig
    pmtools                     # acpidump
    poppler_utils               # pdf2txt
    postgresql                  #just for the psql command
    psmisc                      #killall
    pv
    python3
    python3Packages.xdot
    qpdf
    #remmina                     # rdp
    rpm
    rsync
    ruby_2_1
    rxvt_unicode
    shutter
    simplescreenrecorder
    #smartgithg
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
    wirelesstools               # iwconfig
    wireshark
    xawtv
    xca
    xcalib                      # calibrate colors
    xfontsel
    xlibs.xkill
    xlibs.xwd
    xpdf
    xscreensaver
    xmlsec
    xorg.xdpyinfo
    xorg.xlsfonts               # font for xosd
    xorg.xwininfo
    xosd
    xsane
    youtubeDL
    zbar                        # parse qr codes
  ];

  # List services that you want to enable:
  services.acpid.enable = true;
  services.upower.enable = true;

  #services.mbpfan.enable = true;
  #services.mbpfan.minFanSpeed = 4000;
  #services.mbpfan.lowTemp = 55;   # try ranges 55-63, default is 63
  #services.mbpfan.highTemp = 58;  # try ranges 58-66, default is 66
  #services.mbpfan.maxTemp = 78;   # do not set it > 90, default is 86
  #services.mbpfan.pollingInterval = 1;
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
  # services.xserver.desktopManager.kde5.enable = true;
  services.xserver.desktopManager.xterm.enable = false;
  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.videoDrivers = [ "nouveau" ];
  #services.xserver.videoDrivers = [ "nvidia" ]; #non-free
  #services.xserver.videoDrivers = [ "xf86videointel" ];
  services.xserver.windowManager.xmonad.enable = true;                 # do not remove
  services.xserver.windowManager.xmonad.enableContribAndExtras = true; # do not remove
  services.xserver.windowManager.default = "xmonad";                   # do not remove

  services.locate.enable = true;

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

  hardware.cpu.intel.updateMicrocode = true;
  hardware.facetimehd.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.bluetooth.enable = true;

  hardware.opengl.extraPackages = with pkgs; [ libvdpau-va-gl vaapiVdpau ]; # vaapiIntel

  time.timeZone = "Australia/Sydney";

  #virtualisation.docker.enable = true;
  #virtualisation.virtualbox.host.enable = true;

  nixpkgs.config = {
    #allowBroken = true;
    allowUnfree = true;
    firefox = {
      enableBluejeans = true; #nonfree
      #enableAdobeFlash = true;
      enableGoogleTalkPlugin = true; #nonfree
      #icedtea = true;
    };
    #chromium = {
    # enablePepperFlash = true; # Chromium's non-NSAPI alternative to Adobe Flash
    # enablePepperPDF = true;
    #};
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

  programs.light.enable = true;
  programs.kbdlight.enable = true;

  nix.useSandbox = true;
  nix.buildCores = 4;

  services.nfs.server = {
    enable     = true;
    exports    = ''
      /home/kranium/possplay/puppet-controlrepo *
    '';
    statdPort  = 4000;
    lockdPort  = 4001;
    mountdPort = 4002;
  };

}
