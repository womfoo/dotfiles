# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  AMI = pkgs.haskellPackages.callPackage /home/kranium/AMI-0.1/default.nix { };
  #mycv = pkgs.callPackage /home/kranium/git/bitbucket.org/womfoo/awesome-cv { };
  #ikvm-launch = pkgs.callPackage /home/kranium/git/github.com/womfoo/nix-launch-ikvm { };
  #ldapseed = pkgs.callPackage /home/kranium/darcs/nix-ldapseed/default.nix{ };
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
    '';
    #temporarily disabled move to section above to activate
    #cp ${mycv}/resume.pdf $out/private
  };
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./telegraf.nix
    ];

  boot.kernelPackages = pkgs.linuxPackages_latest;   # 4.12 as of 2017-Sep-10
  #boot.kernelPackages = pkgs.linuxPackages_4_12;    # works so far
  #boot.kernelPackages = pkgs.linuxPackages_testing; # 4.13-rc7 as of 2017-Sep-10

  # Use the gummiboot efi boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.timeout = 4;
  boot.loader.efi.canTouchEfiVariables = true;

  networking = {
    hostName = "silverspark";
    networkmanager.enable = true;
    #disabling for now, reenable when in a country that censors free speech
    #networkmanager.insertNameservers = [ "8.8.8.8" "8.8.4.4" ];
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
    cloudmonkey
    facter
    fnotifystat
    forkstat
    gpxsee
    pick
    powerstat
    smemstat
    xzgv
    # work
    hipchat
    skype
    slack
    # local
    #ikvm-launch
    #ldapseed
    # mine
    abcde
    abiword
    acd-cli
    acpi
    adapta-gtk-theme
    ag
    aircrack-ng
    androidsdk
    #ant
    ansible
    arc-theme
    arandr
    aria2
    asciinema
    audacity
    augeas
    avidemux
    awscli
    baresip
    #baobab
    bind                        # nslookup
    binutils                    # ld, ar
    bitcoin
    blueman
    bmon
    btrfs-progs
    #bundix
    #bundler
    sox
    ffmpeg
    calibre
    chromium
    google-chrome
    #google-chrome-beta
    #google-chrome-dev
    cifs_utils
    compton
    conkeror
    cool-retro-term
    cpuminer-multi
    cryptsetup
    darcs
    #deadbeef
    debootstrap
    dillo
    dmenu
    docker_compose
    dropbox
    duc
    dpkg # so we can view files inside debs
    (with emacsPackagesNg; emacsWithPackages [
      vagrant-tramp
      powerline
      ivy-hydra
      #smart-mode-line
      #helm
      ivy
      swiper
      counsel
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
    ekiga
    electrum
    exfat
    exfat-utils
    #facter
    #fbreader
    file
    firefox-esr # NPAPI support until 2018 for hangouts.
    #firefox-bin # no hangouts?
    #firefox-esr
    #firefox-beta-bin
    flac
    fuse_exfat
    geoip
    ghostscript                 # needed by emacs doc-view
    gimp
    gitFull
    glxinfo
    gnome3.cheese
    #gnome3.eog
    gnome3.evince              # not built in unstable-small <2016-11-05>
    gnome3.nautilus
    #gnucash                    # not built in unstable-small <2016-11-04>
    #gnumeric
    gnupg1compat
    go-mtpfs                    # jmtpfs and mtpfs fails on my xiaomi
    #gnutls
    gpa
    gparted
    gpodder
    gptfdisk
    gtkgnutella
    #gtkpod
    graphviz
    gsmartcontrol
    #stack
    hledger
    hledger-web
    #(haskell.packages.ghc7103.ghcWithPackages (self : with haskell.packages.ghc7103; with pkgs.haskell.lib; [
    (haskellPackages.ghcWithPackages (self : with haskellPackages; with pkgs.haskell.lib; [
      propellor
      yaml-light
      AMI
      #tinfoil
      ble
      xmobar
      cabal-install
      cabal2nix
      sproxy2
      mywatch
      weeder
      #mywatch zalora mysql
    #(haskell.packages.ghc7103.ghcWithPackages (self : with haskell.packages.ghc7103; with pkgs.haskell.lib; [
      #update-nix-fetchgit
      #github
      #mygithub
      stylish-haskell
      hlint
      #(self.callPackage /home/kranium/git/github.com/womfoo/github/default.nix { })
      text-conversions
      #servant-github
      servant
      mysql-haskell
      mysql-simple
      git
      hGelf
      brick
      markdown-unlit
      hail
      #hsass
      #webdriver
      ShellCheck
      #alex
      #cabal-install
      #cabal2nix
      ghc-mod
      hledger
      #wreq
      xmobar
      #hnix
      #hnix_loc
      #hGelf
      #gender
      #hakyll
      #hakyll-sass
      #aeson-pretty
      #stack
      #hails
      timeplot
      splot
      language-puppet
    ]))
    hfsprogs
    hiera-eyaml
    htop
    #ifuse
    imagemagick
    inetutils
    iptraf-ng
    #innoextract
    #inkscape
    #inotify-tools
    iotop
    iw                          # iw list
    jitsi
    jq
    #kazam
    #kde4.digikam
    #kde4.gwenview
    #kde4.kdenlive
    #kde4.kdiff3
    #kde4.ktorrent
    #kde4.okular
    #kde5.okular
    #kdiff3-qt5  #broken 2017-06-25
    #ktorrent    #broken 2017-06-25
    keepassx
    keybase
    kops
    kpcli
    kubernetes
    #libreoffice # broken 20170802
    libva-full                  # vaapiVdpau should installt this but I need vainfo
    #libvdpau-va-gl              # vdpauinfo
    linphone
    vdpauinfo                   # lol
    libxml2                     #xmllint
    libxslt
    #logstash
    lsof
    #lxc
    mc                          # not built in unstable-small <2016-11-04>
    meld
    #mercurialFull
    minikube
    monero
    mosh
    #mplayer
    (mtr.override { withGtk = true; })
    mysql-workbench
    ncdu
    netdata
    nethogs
    #netsurf
    networkmanager_openconnect
    networkmanager_pptp
    networkmanagerapplet
    nix-prefetch-git
    nix-repl
    nodePackages.node2nix #creates hugeass file
    #nox # broken Jun 10 2017
    npm2nix
    ntfs3g
    oathToolkit
    #openjdk
    openconnect_openssl
    openldap                    # ldapsearch
    openssl
    #openttd
    pandoc
    parcellite
    #parted
    patchelf
    pavucontrol
    pciutils                    # setpci
    #pdftk
    pdfmod
    #pgadmin
    p7zip
    pianobar
    #pidgin
    pipes                       # screensaver
    pkgconfig
    pmtools                     # acpidump
    poppler_utils               # pdf2txt
    postgresql                  #just for the psql command
    psmisc                      #killall
    pv
    #python3
    #python3Packages.xdot
    python3
    python3Packages.pip
    python3Packages.selenium
    python3Packages.binwalk
    qpdf
    remmina                     # rdp
    rpm
    rsync
    ruby_2_1
    rxvt_unicode
    screen
    scrot                        # screenshot tool
    shared_mime_info
    shutter
    simplescreenrecorder
    sipcmd
    #smartgithg
    smartmontools
    spaceFM
    speedtest-cli
    #sqlite
    #squashfsTools
    #sshfsFuse
    #subversion
    tcpdump
    terminator
    terraform
    #tesseract
    thunderbird
    tora
    torbrowser
    trayer
    tree
    unrar
    unzip
    usbutils                    # lsusb
    vagrant
    veracrypt
    virtualbox
    vlc
    vnstat
    wget
    which
    wirelesstools               # iwconfig
    #wireshark
    #xawtv
    #xca                         # certificate authority gui
    xcalib                      # calibrate colors
    xfontsel
    xlibs.xkill
    xlibs.xwd
    xpdf
    xscreensaver
    xmlsec
    xdotool
    xorg.xauth
    xorg.xdpyinfo
    xorg.xlsfonts               # font for xosd
    xorg.xwininfo
    xosd
    xsane
    youtubeDL
    zbar                        # parse qr codes
    zip
  ];

  environment.etc = {
    "libao.conf".text = ''
      default_driver=pulse
    '';
  };

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
  powerManagement.cpuFreqGovernor = "performance";

  hardware.facetimehd.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.package = pkgs.pulseaudioFull; # we need to use the full package for bluetooth support
  #for wireless headset
  hardware.pulseaudio.extraConfig = ''
    load-module module-switch-on-connect
  '';

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

  services.dnsmasq = {
    enable = true;
    extraConfig = ''
      addn-hosts=/etc/hosts.vagrant-hosts
    '';
  };

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
