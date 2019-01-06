# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, options, pkgs, ... }:

let
  #xmobar = pkgs.haskell.lib.justStaticExecutables pkgs.haskell.packages.ghc822.xmobar;
  xmobar = pkgs.haskell.lib.justStaticExecutables pkgs.haskell.packages.ghc861.xmobar;
  AMI = pkgs.haskellPackages.callPackage /home/kranium/AMI-0.1/default.nix { };
  mycv = pkgs.callPackage /home/kranium/git/bitbucket.org/womfoo/awesome-cv { };
  #ikvm-launch = pkgs.callPackage /home/kranium/git/github.com/womfoo/nix-launch-ikvm { };
  #ldapseed = pkgs.callPackage /home/kranium/darcs/nix-ldapseed/default.nix{ };
  #hnix_loc = pkgs.callPackage /home/kranium/git/github.com/jwiegley/hnix/default.nix { };
  idpmetadata = pkgs.fetchurl {
    url = "https://kranium.oktapreview.com/app/exk5sig0ciaHGuguQ0h7/sso/saml/metadata";
    sha256 = "023bvc32dw4wcxn53b38rl7mbyb5bh5vl3dfhschjb100g61a979";
  };
  # basic auth testing
  # user: user
  # password: swordfish
  basicPasswordFile = pkgs.writeText "htpasswd" ''
  user:$apr1$/ZZCxKOB$C7SdA24Qn6D9w9N0CAgiC/
  '';
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
      mkdir -p $out/basic_auth
      echo 'hello basic auth' > $out/basic_auth/hello.txt
      cp localhost.key $out/private
      cp localhost.cert $out/public
      cp localhost.xml $out/public
      cp ${mycv}/resume.pdf $out/private
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

  boot.kernelPackages = pkgs.linuxPackages_latest;
  #boot.kernelPackages = pkgs.linuxPackages_4_18;
  #boot.kernelPackages = pkgs.linuxPackages_testing;

  # Use the gummiboot efi boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.timeout = 4;
  boot.loader.efi.canTouchEfiVariables = true;

  networking = {
    hostName = "silverspark";
    networkmanager.enable = true;
    # networkmanager.useDnsmasq = true; # compat generation <= 1898
    networkmanager.dns = "dnsmasq";
    # wicd.enable = true;
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
    libva-utils
    # local-override
    xmobar # currently segfaults
    # maintainted
    cloudmonkey
    facter
    find-cursor
    fnotifystat
    forkstat
    gpxsee
    pick
    powerstat
    smemstat
    xzgv
    yq
    # work
    #hipchat
    #skype
    skypeforlinux
    #slack                     # too much of a resource hog
    # local
    #ikvm-launch
    #ldapseed
    # mine
    abcde
    abiword                    # no binary on master 2017-11-04
    acd-cli
    acpi
    #adapta-gtk-theme
    #afl                       # cant remember why i installed this commenting
    ag
    aircrack-ng
    #androidsdk                # no binary on master 2017-11-04
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
    baresip                    # unstable broken 20171114
    baobab
    bind
    binutils                    # ld, ar
    bitcoin #failing jan 5 2018
    #bonfire                     #not in 17.09
    blueman
    bmon
    btrfs-progs
    bundix
    #bundler
    sox
    ffmpeg
    calibre
    cfssl
    chromedriver
    chromium
    cli53
    docker_compose
    google-chrome
    #google-chrome-beta
    google-chrome-dev          # facetimehd works here https://github.com/patjak/bcwc_pcie/issues/123
    cifs_utils
    compton-git
    #conkeror                  # apr 16 2018 firefox esr dead
    conntrack_tools
    cool-retro-term
    cpuminer-multi
    cryptsetup
    darcs
    #deadbeef
    dbeaver
    debootstrap
    dillo
    dmenu
    #docker_compose
    dropbox
    duc
    dnsutils                    # nslookup
    dpkg # so we can view files inside debs
    emacs
    elfutils
    encfs
    ec2_api_tools
    ec2_ami_tools
    ecdsautils
    ekiga
    #electrum
    #/nix/store/gahaavibp60fy15yd60wl8w5fx07437y-electrum-3.1.3/bin/electrum
    #etherape
    exfat
    exfat-utils
    facter
    #fbreader
    file
    #firefox
    #firefox-esr # NPAPI support until 2018 for hangouts, takes a long time to build 20160
    firefox-bin # no hangouts?
    #firefox-esr
    #firefox-beta-bin # 57 is out!
    flac
    flameshot
    fpm
    fuse_exfat
    geoip
    #ghcid #does not work here
    ghostscript                 # needed by emacs doc-view
    gimp
    gitFull
    gnome3.librsvg
    go
    go2nix
    gx
    gx-go
    glxinfo
    gnome3.adwaita-icon-theme
    gnome3.cheese
    #gnome3.eog
    gnome3.evince              # not built in unstable-small <2016-11-05>
    #gnome3.file-roller
    #gnome3.nautilus
    #gnucash                    # not built in unstable-small <2016-11-04>
    #gnumeric
    gnupg1compat
    go-mtpfs                    # jmtpfs and mtpfs fails on my xiaomi
    #gnutls
    gpa
    gparted
    gpodder
    gptfdisk
    #gtkgnutella
    #gtkpod
    graphviz
    gsmartcontrol
    #(with gst_all_1; [ gst-plugins-base gst-plugins-good gst-plugins-ugly ])
    #gst_all_1.gst-plugins-base
    #gstreamer
    #gst-plugins-base
    gst_all_1.gstreamer
    gst_all_1.gst-plugins-base
    gst_all_1.gst-plugins-good
    gst_all_1.gst-plugins-bad
    gst_all_1.gst-plugins-ugly
    #stack
    hledger
    hledger-web
    #(haskell.packages.ghc7103.ghcWithPackages (self : with haskell.packages.ghc7103; with pkgs.haskell.lib; [
    #(haskell.packages.ghc802.ghcWithPackages (self : with haskell.packages.ghc802; with pkgs.haskell.lib; [
    #(haskell.packages.ghcjs.ghcWithPackages (self : with haskell.packages.ghcjs; with pkgs.haskell.lib; [
    # (haskell.packages.ghcjs.ghcWithPackages (self : with haskell.packages.ghcjs; [
    #    ghcjs-dom
    # ]))
    (haskellPackages.ghcWithPackages (self : with haskellPackages; with pkgs.haskell.lib; [
      #questioner # ansi-terminal
      #teleport does not build with 8.4
      #hog
      HsOpenSSL
      hGelf
      #http-client
      #http-client-tls
      #scotty
      # slack-api
      # optparse-applicative
      # cryptonite
      http-conduit
      hamlet
      # #intero
      # #snap
      # #snap-templates
      # #update-nix-fetchgit
      # #jsons-to-schema
      # wuss
      # websockets
      # #conduit-audio # probably failing due to mpd
      # #pulse-simple # probably failing due to mpd
      # #mediabus-rtp
      # propellor
      # yaml-light
      # AMI
      # #tinfoil
      # #ble # needs patch dbus
      #xmobar
      cabal-install
      cabal2nix
      # sproxy2
      # mywatch
      # weeder
      # #mywatch zalora mysql
      # #update-nix-fetchgit
      # #github
      # #mygithub
      stylish-haskell
      hlint
      yeganesh
      # #(self.callPackage /home/kranium/git/github.com/womfoo/github/default.nix { })
      # text-conversions
      # #servant-github
      # servant
      # #mysql-haskell # binary-parsers fails
      # #mysql-simple # binary-parsers fails
      # git
      # hGelf
      # brick
      # markdown-unlit
      # #hail #tasty-quickcheck ==0.8.*
      # #hsass
      # #webdriver
      # ShellCheck
      # #alex
      # #cabal-install
      # #cabal2nix
      #ghc-mod # cabal-helpernotworking
      # hledger
      # #wreq
      # xmobar
      #hnix
      #hnix_loc
      #hGelf
      #gender
      #hakyll
      #hakyll-sass
      #aeson-pretty
      #stack
      #hails
      # timeplot
      #splot  # broken by latest ghc
      #language-puppet   # broken 20180325
      #xdot
    ]))
    hfsprogs
    hiera-eyaml
    hicolor-icon-theme
    htop
    httpie
    #ifuse
    imagemagick
    inetutils
    influxdb
    iperf
    iptraf-ng
    #innoextract
    #inkscape
    inotifyTools
    iotop
    ispell
    iw                          # iw list
    jetbrains.idea-community
    jitsi
    jmeter
    jwhois
    jq
    #kazam
    #kde4.digikam
    #kde4.gwenview
    #kde4.kdenlive
    #kde4.kdiff3
    #kde4.ktorrent
    #kde4.okular
    #kde5.okular
    kdiff3-qt5
    #ktorrent    #broken 2017-06-25
    keepassx
    keybase
    keybase-gui
    kitty                            # gpu terminal FTWb # not in 17.09
    kops
    kpcli
    kubernetes  # tests failing 2018-01-03
    kubectl
    languagetool
    libreoffice                # 16 apr 2018
    librarian-puppet-go
    libva-full                  # vaapiVdpau should installt this but I need vainfo
    #libvdpau-va-gl              # vdpauinfo
    linphone
    vdpauinfo                   # lol
    libxml2                     #xmllint
    libxslt
    lmdb                        # mdb_copy for backing up monero
    #logstash
    lsof
    #lxc
    mc                          # not built in unstable-small <2016-11-04>
    meld
    #mercurialFull
    minikube
    monero
    mosh
    mplayer
    mpv
    (mtr.override { withGtk = true; })
    mumble
    mysql
    #mysql-workbench   # broken on master 2018-01-21
    ncdu
    neovim
    netdata
    nethogs
    #netsurf
    networkmanager_openconnect
    #networkmanager_pptp  #gone 18.03
    networkmanager_l2tp
    networkmanagerapplet
    ngrep
    nix-prefetch-git
    #nix-repl
    nmap
    nodePackages.bower
    nodePackages.node2nix #creates hugeass file
    nodePackages.pulp # not in 17.09
    nodejs
    #nox # broken Jun 10 2017
    npm2nix
    ntfs3g
    oathToolkit
    #openjdk
    openconnect_openssl
    openldap                    # ldapsearch
    #openra
    openssl
    #openttd
    packer
    pandoc
    parcellite
    pass
    #parted
    patchelf
    pasystray
    pavucontrol
    pciutils                    # setpci
    #pdftk
    #pdfmod
    pgadmin
    p7zip
    pianobar
    #pidgin
    picocom
    pipes                       # screensaver
    pkgconfig
    pmtools                     # acpidump
    poppler_utils               # pdf2txt
    postgresql                  #just for the psql command
    ppp
    pptp
    psmisc                      #killall
    #purescript
    pv
    #python3
    #python3Packages.xdot
    python3
    python3Packages.pip
    #python3Packages.selenium   # fail 2018022
    python3Packages.binwalk
    #python36Packages.azure-cli
    python36Packages.gunicorn
    #python2Packages.xdot
    qemu
    qpdf
    ranger
    redshift
    remmina                     # rdp
    rhash
    rpm
    rsync
    r10k
    #ruby_2_1                    # deprecated in 17.09
    ruby_2_4
    runc
    rxvt_unicode-with-plugins
    screen
    scrot                        # screenshot tool
    shared_mime_info
    shutter
    simplescreenrecorder
    signal-desktop               # not in 17.09
    #sipcmd                        # wont build 25mar2018 xb xb
    sipp
    sipsak
    softether
    sshpass
    steam
    subversionClient
    #smartgithg
    smartmontools
    spaceFM
    speedtest-cli
    #stack2nix                 # wont build 4jul2018
    #strongswan
    #sqlite
    sysstat                    # iotop etceel
    wxsqlite3
    wxsqliteplus
    #squashfsTools
    #sshfsFuse
    #subversion
    texlive.combined.scheme-full
    tilix
    #tetex                      # pandoc md to pdf needs this
    tcpdump
    terminator
    terraform
    #tesseract
    thunderbird
    #tora
    torbrowser
    tmux
    tmux-cssh
    trayer
    tree
    ####unstable brave
    ####tsung
    #unrar                      # no unstable binary 20171114
    unzip
    usbutils                    # lsusb
    vagrant
    veracrypt
    vim
    virtualbox
    vlc
    #vscode
    vnstat
    wget
    which
    wire-desktop
    wirelesstools               # iwconfig
    wireshark
    wkhtmltopdf
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
    xorg.xhost
    xorg.xlsfonts               # font for xosd
    xorg.xwininfo
    xosd
    xclip
    xsane
    youtubeDL
    zbar                        # parse qr codes
    zsync
    zip
  ];

  #environment.etc = {
  #  "libao.conf".text = ''
  #    default_driver=pulse
  #  '';
  #};
  };

  services.dovecot2.enable = true;

  environment.variables = {
      GTK_DATA_PREFIX = "/run/current-system/sw";
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
  services.printing.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";
  services.xserver.libinput.enable = true;
  services.xserver.synaptics.enable = false;
  # services.xserver.synaptics.enable = true;
  # services.xserver.synaptics.palmDetect = true;
  # services.xserver.synaptics.fingersMap = [1 3 2];
  # services.xserver.synaptics.twoFingerScroll = true;
  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.kdm.enable = true;
  # services.xserver.desktopManager.kde4.enable = true;
  # services.xserver.desktopManager.kde5.enable = true;
  services.xserver.desktopManager.xterm.enable = false;
  #services.xserver.displayManager.lightdm.enable = true;   #the real deal
  services.xserver.displayManager.sddm.enable = true;   #the real deal
  #services.xserver.videoDrivers = [ "nouveau" ];
  services.xserver.deviceSection = ''
     # only for nouveau
     ###Option "GLXVBlank" "on"
     ###Option "DRI" "3"
     #Option "SwapLimit" "2" #makes it worse
  '';

  services.xserver.screenSection = ''
    Option "metamodes" "nvidia-auto-select +0+0 { ForceFullCompositionPipeline = On }"
    Option         "AllowIndirectGLXProtocol" "off"
    Option         "TripleBuffer" "on"
  '';

  #services.xserver.videoDrivers = [ "nvidia-beta" ];  #non-free
  services.xserver.videoDrivers = [ "nvidia" ];       #non-free <-- faster but breaks ttys and brightness keys
  #services.xserver.videoDrivers = [ "xf86videointel" ];
  services.xserver.windowManager.xmonad.enable = true;                 # do not remove
  services.xserver.windowManager.xmonad.enableContribAndExtras = true; # do not remove
  services.xserver.windowManager.default = "xmonad";                   # do not remove

  services.locate.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.kranium = {
     name = "kranium";
     extraGroups = [ "wheel" "networkmanager" "audio" "docker" "vboxusers" "video" "lp" ];
     group = "users";
     uid = 2000;
     createHome = true;
     home = "/home/kranium";
     shell = "/run/current-system/sw/bin/bash";
  };
  users.extraGroups = { networkmanager = { } ; kranium = { gid = 2000; } ; } ;

  hardware.cpu.intel.updateMicrocode = true;

  #this tends to overheat
  powerManagement.cpuFreqGovernor = "performance";

  hardware.facetimehd.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.package = pkgs.pulseaudioFull; # we need to use the full package for bluetooth support
  #for wireless headset
  hardware.pulseaudio.extraConfig = ''
    load-module module-switch-on-connect
  '';

  hardware.bluetooth.enable = true;
  hardware.bluetooth.extraConfig  = ''
    [General]
    AutoConnect=true
    Name = %h-%d
  '';
  # for steam to work
  hardware.opengl = {
    driSupport = true;
    driSupport32Bit = true;
  };
  #hardware.opengl.extraPackages = with pkgs; [ libvdpau-va-gl vaapiVdpau ]; # vaapiIntel
  #hardware.opengl.extraPackages = with pkgs; [ vaapiVdpau ]; # vaapiIntel

  time.timeZone = "Australia/Sydney";

  #virtualisation.rkt.enable = true;
  virtualisation.docker.enable = true;
  virtualisation.virtualbox.host.enable = true;

  nixpkgs.config = {
    allowBroken = true;
    allowUnfree = true;
    #firefox = {
    #  #enableAdobeFlash = true;
    #  enableGoogleTalkPlugin = true; #nonfree
    #  #icedtea = true;
    #};
    chromium = {
    # enablePepperFlash = true; # Chromium's non-NSAPI alternative to Adobe Flash
    enableAdobeFlash = true;
    enablePepperPDF = true;
    #overlays = [ "/home/kranium/git/github.com/stesie/azure-cli-nix/" ];
    };

    #permittedInsecurePackages = [
    #     "linux-4.13.16"
    #];

  };

  security.sudo.wheelNeedsPassword = false;

  # services.kbfs = {
  #   enable = true;
  #   mountPoint = "/keybase";
  # };
  # services.keybase.enable = true;

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

        <Location /basic_auth>
            AuthName "Please Log In"
            AuthType Basic
            require valid-user
            AuthUserFile ${basicPasswordFile}
        </Location>
        '';
       }
    ];
  };

  users.extraUsers.wwwrun.extraGroups = ["transmission"];

  programs.java.enable = true;

  programs.light.enable = true;
  programs.kbdlight.enable = true;

  nix.useSandbox = true;
  nix.buildCores = 4;

  nix.binaryCaches = [
    "https://cache.nixos.org/"
    "https://nixcache.reflex-frp.org"
    "https://static-haskell-nix.cachix.org"
  ];
  nix.binaryCachePublicKeys = [
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
    "static-haskell-nix.cachix.org-1:Q17HawmAwaM1/BfIxaEDKAxwTOyRVhPG5Ji9K3+FvUU="
  ];
  # https://nixos.wiki/wiki/Overlays
  nix.nixPath = options.nix.nixPath.default ++
  [ "nixpkgs-overlays=/etc/nixos/overlays-compat/" ]
  ;

  services.dnsmasq = {
    enable = true;
    extraConfig = ''
      addn-hosts=/etc/hosts.vagrant-hosts
    '';
  };

  #services.dockerRegistry.enable = true;

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
