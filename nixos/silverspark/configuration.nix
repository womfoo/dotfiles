# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, options, pkgs, ... }:

let

  myphone-numbers = pkgs.haskell.lib.overrideCabal pkgs.haskellPackages.phone-numbers (drv: {

    configureFlags = (drv.configureFlags or []) ++ [
      "--extra-lib-dirs=${pkgs.libphonenumber}/lib"
      "--extra-include-dirs=${pkgs.libphonenumber}/include"
      ];
    #preConfigure = "sed -i -e 's/extra-libraries: phonenumber/extra-libraries: libphonenumber/g' phone-numbers.cabal";
  });

  install1903Apps = false;

  newApps = with pkgs; [
    azure-storage-azcopy
    brave
    kdeApplications.konqueror
    tsung
  ];

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
      ln -s ${pkgs.linuxPackages.virtualboxGuestAdditions.src} $out/public/vbox.iso
      ln -s /home/kranium/Downloads $out/private/Downloads
      cp ${mycv}/resume.pdf $out/private
    '';
  };
  secrets = import ./secrets.nix;
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./telegraf.nix
      ./work.nix
      # ./asterisk-test.nix
    ];

  # boot.kernelPackages = pkgs.linuxPackages_latest;
  # boot.kernelPackages = pkgs.linuxPackages_5_0;
  boot.kernelPackages = pkgs.linuxPackages_4_19;
  # boot.kernelPackages = pkgs.linuxPackages_testing;

  # Use the gummiboot efi boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.timeout = 4;
  boot.loader.efi.canTouchEfiVariables = true;

  networking = {
    # nameservers = [ "8.8.8.8" "8.8.4.4" ];
    hostName = "silverspark";
    networkmanager.enable = true;
    # networkmanager.useDnsmasq = true; # compat generation <= 1898
    # networkmanager.dns = "dnsmasq"; # kranium
    # wicd.enable = true;
    #disabling for now, reenable when in a country that censors free speech
    # networkmanager.insertNameservers = [ "8.8.8.8" "8.8.4.4" ];
    # networkmanager.unmanaged = [ "ens9" ];
    firewall.allowedTCPPorts = [
      22
      # 80                     # http
      2049 # nfs
      4000 # nfs/statd
      4001 # nfs/lockd
      4002 # nfs/mountd
      # 5060                   # sip
      # 5432                   # postgres
    ];
    firewall.allowedUDPPorts = [
      53                       # dns
      67                       # udp client -> server
      # 68                     # udp server ->
      2049 # nfs
      4000 # nfs/statd
      4001 # nfs/lockd
      4002 # nfs/mountd
      # 5060                   # sip
    ];
    firewall.allowedUDPPortRanges = [ { from = 10000; to = 15000; } ];

    extraHosts = ''
      127.0.0.1 voipmonitor.org
      127.0.0.1 www.voipmonitor.org
      127.0.0.1 download.voipmonitor.org
      127.0.0.1 cloud.voipmonitor.org
      127.0.0.1 cloud2.voipmonitor.org
      127.0.0.1 cloud3.voipmonitor.org
    '';
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
       discord
       teams
       #jre8Plugin
       #adobe-reader
       certbot
       azure-cli
       azure-storage-azcopy
       arduino
       #ffmpegthumbnailer
       darktable
       fcitx
    libva-utils
    # local-override
    # maintainted
    cloudmonkey
    facter
    #find-cursor
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
    #skypeforlinux             # 2019-11-06 error: cannot download skypeforlinux_8.51.0.72_amd64.deb from any mirror
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
    arora
    aria2
    asciinema
       # audacity # broken b50ef9a aug 14 2020
    augeas
    avidemux
    awscli
       # baresip # unstable broken 20171114
    baobab
       bettercap
    bind
    binutils                    # ld, ar
    bitcoin #failing jan 5 2018
    #bonfire                     #not in 17.09
    #blueman
    bluez-tools                # bt-device --list
    bmon
    btrfs-progs
    bundix
    #bundler
    sox
    ffmpeg
       # ffmpegthubnailer
       # calibre # error: dnspython-2.0.0 not supported for interpreter python2.7
    cfssl
    chromedriver
    chromium                  # broke 30 jul 2019 last gen /nix/store/2hiqxhqrjfgzw93fvr0dcrlq44vszj2d-chromium-75.0.3770.90/bin/chromium
    cli53
       spotify
    docker_compose
    google-chrome
    #google-chrome-beta
    google-chrome-dev          # facetimehd works here https://github.com/patjak/bcwc_pcie/issues/123
    # gqrx                       # sdr
    cifs_utils
    compton-git
    #conkeror                  # apr 16 2018 firefox esr dead
    conntrack_tools
    cool-retro-term
    cpuminer-multi
    cryptsetup
       # darcs
    #deadbeef
    # dbeaver
    debootstrap
    dillo
       # (dillo.override { openssl = openssl_1_0_2; })
    dmenu
    #docker_compose
    dropbox
    duc
    dnsutils                    # nslookup
    dos2unix
    dpkg # so we can view files inside debs
    emacs
    elfutils
    encfs
    ec2_api_tools
    ec2_ami_tools
    ecdsautils
       # ekiga # ftbs, need boost?
       # electrum                    # fail
    #/nix/store/gahaavibp60fy15yd60wl8w5fx07437y-electrum-3.1.3/bin/electrum
    #etherape
    exfat
    exfat-utils
    f2fs-tools
    facter
    #fbreader
    file
    firefox
    # /nix/store/mppf21vfr0sc7gy6fbcv5gfpgj43ib24-firefox-74.0/bin/firefox
    #firefox-esr # NPAPI support until 2018 for hangouts, takes a long time to build 20160
       # firefox-bin # no hangouts?
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
    gitAndTools.hub
    gnome3.librsvg
    # gns3-gui                 # broken in unstable
    # gns3-server              # broken in unstable
    go
    go2nix
    go-jira
    gx
    gx-go
    glxinfo
    gnome3.adwaita-icon-theme
       gnome3.cheese
    #gnome3.eog
    gnome3.evince              # not built in unstable-small <2016-11-05>
    gnome3.file-roller
    #gnome3.nautilus
    #gnucash                    # not built in unstable-small <2016-11-04>
    #gnumeric
       # gnupg1compat
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
       # haskell.packages.ghc865.darcs
    (haskellPackages.ghcWithPackages (self : with haskellPackages; with pkgs.haskell.lib; [
      myphone-numbers
      # espial                 # haddock fail
      arbtt
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
      # hamlet
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
      xmobar
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
      mysql-haskell
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
    hwloc
    #ifuse
    imagemagick
    inetutils
    influxdb
    ipcalc
    iperf
    iptraf-ng
    #innoextract
       inkscape
    inotifyTools
    iotop
    ispell
    iw                          # iw list
    jetbrains.idea-community
       # jitsi # broken 16-apr-2020, crap gui anyways
    # jira-cli
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
    okular
    kdiff3-qt5
    ktorrent
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
    libnotify                   # notify-send pp
    libphonenumber
    librarian-puppet-go
    libva-full                  # vaapiVdpau should installt this but I need vainfo
    #libvdpau-va-gl              # vdpauinfo
    # linphone
    vdpauinfo                   # lol
    libxml2                     #xmllint
    libxslt
    lmdb                        # mdb_copy for backing up monero
       lm_sensors
    #logstash
    lsof
    #lxc
    mc                          # not built in unstable-small <2016-11-04>
       masterpdfeditor # perl broken 2020-06-13
    meld
    #mercurialFull
    minicom
       # minikube # too big
    monero
    mosh
       # mplayer
    msf
    mpv
    (mtr.override { withGtk = true; })
    # mumble
    mysql
       # mysql-workbench # broken on master 2018-01-21 # paramiko error: bcrypt-3.2.0 not supported for interpreter python2.7
    ncdu
    neovim
       net_snmp
    netdata
    nethogs
    netsurf.browser
    networkmanager_openconnect
    #networkmanager_pptp  #gone 18.03
    networkmanager_l2tp
    networkmanagerapplet
    ngrep
    nix-index
    nix-prefetch-git
       nix-top
    nixops
    nixfmt
    #nix-repl
    nmap
    nmap-graphical
       # nodePackages.bower
       # nodePackages.node2nix #creates hugeass file
       # nodePackages.pulp # not in 17.09
       # nodePackages.tern
       # nodejs
    #nox # broken Jun 10 2017
    # npm2nix                  # deprecated, node2nix or yarn2nix
    ntfs3g
    oathToolkit
    #openjdk
    openconnect_openssl
    openldap                    # ldapsearch
    #openra
    openssl
       # otter-browser # removed 20.03?
    #openttd
    packer
    pandoc
    parcellite
    pass
       parallel
    #parted
    patchelf
    pasystray
    pavucontrol
    pciutils                    # setpci
    pdfcrack
    pdftk
    #pdfmod
       # pgadmin
       # p7zip # marked as insecure 
    pianobar
    #pidgin
    picocom
    pipes                       # screensaver
    pkgconfig
    pmtools                     # acpidump
    poppler_utils               # pdf2txt
    postgresql                  #just for the psql command
       postman
    ppp
    pptp
       procmail # lockfile 
    psmisc                      #killall
    pssh
    #purescript
    pulsemixer
    pv
    #python3
    #python3Packages.xdot
    python3
    python3Packages.pip
    #python3Packages.selenium   # fail 2018022
    python3Packages.binwalk
    python3Packages.sqlparse
    #python36Packages.azure-cli
    python36Packages.gunicorn
    #python2Packages.xdot
    pypi2nix
    qemu
    qpdf
    ranger
    redshift
    remmina                     # rdp
    rhash
    rpm
    rsync
    r10k
       ruby_2_6
    rrdtool
    # rtl-sdr
    runc
    rxvt_unicode-with-plugins
    screen
    scrot                        # screenshot tool
    shared_mime_info
    shellcheck
    gnome3.seahorse                     # edit items in gnome-keyring 
    # shutter                  # gtk2 perl not building in unstable 28 oct 2019
    simplescreenrecorder
    signal-desktop               # not in 17.09
    #sipcmd                        # wont build 25mar2018 xb xb
    sipp
    sipsak
    softether
    sshpass
    #steam
    subversionClient
    #smartgithg
    smartmontools
    spaceFM
    speedtest-cli
    st
       # (st.override { conf = builtins.readFile ./config.def.h; })
    #stack2nix                 # wont build 4jul2018
    #strongswan
    sqlite
    sqlitebrowser
    sysstat                    # iotop etceel
    wxsqlite3
    wxsqliteplus
    #squashfsTools
    #sshfsFuse
    #subversion
    texlive.combined.scheme-full
    # tilix
    #tetex                      # pandoc md to pdf needs this
    tcpdump
    # teamviewer               # fails in 19.09?
    terminator
    terraform
    #tesseract
    thunderbird
    #tora
    torbrowser
    # tmux
    tmux-cssh
    trayer
    tree
    #unrar                      # no unstable binary 20171114
    unzip
    usbutils                    # lsusb
    vagrant
    veracrypt
    vim
       # vivaldi
    #virtualbox                 # do not enable virtualisation.virtualbox.host.enable = true is enough. weird erros occur.
    vlc
    vulnix
    vscode
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
       # xpdf # CVE
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
    yate
    youtubeDL
    zbar                        # parse qr codes
       zfs
       zoom-us # compiling feb 2020
    zsync
    zip
  ] ++ lib.optional install1903Apps newApps;

  #environment.etc = {
  #  "libao.conf".text = ''
  #    default_driver=pulse
  #  '';
  #};

  services.dovecot2.enable = true;

  environment.variables = {
      GTK_DATA_PREFIX = "/run/current-system/sw";
    };
  # List services that you want to enable:
  services.acpid.enable = true;
  services.upower.enable = true;

  services.mbpfan.enable = true;
  services.mbpfan.minFanSpeed = 4000; # noisy
  # services.mbpfan.lowTemp = 55;   # try ranges 55-63, default is 63
  # services.mbpfan.highTemp = 58;  # try ranges 58-66, default is 66
  # services.mbpfan.maxTemp = 78;   # do not set it > 90, default is 86
  # services.mbpfan.pollingInterval = 1;
  # services.mbpfan.verbose = true;

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers = [ pkgs.fxlinuxprint ];

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
     extraGroups = [ "wheel" "networkmanager" "audio" "docker" "vboxusers" "video" "lp" "dialout" "libvirtd" ];
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
  services.flatpak.enable = true;
  xdg.portal.enable = true;
  virtualisation.virtualbox.host.enable = true;
  virtualisation.libvirtd.enable = true;
  # services.dockerRegistry.enable = true;

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
    # enableAdobeFlash = true;
    # enablePepperPDF = true;
    };
    overlays = [ "/home/kranium/git/github.com/stesie/azure-cli-nix/" ];
    #packageOverrides = super: let self = super.pkgs;
    /*
    packageOverrides = super:
      let osxBlob = pkgs.requireFile {
        message = ''
          nix-prefetch-url file:///MacOSX/System/Library/Extensions/AppleCameraInterface.kext/Contents/MacOS/AppleCameraInterface
        '';
        sha256 = "0zb52vsv04if2kla5k2azwfi04mn8mmpl5ahkgpch0157byygb4x";
        name = "AppleCameraInterface";
      };
      in {
      facetimehd-firmware = super.facetimehd-firmware.overrideDerivation (old: {
        name = "facetimehd-firmware";
        src = null; # disables download from apple
        buildPhase = ''
          mkdir -p $out/lib/firmware/facetimehd
          dd bs=1 skip=81920 if=${osxBlob} | gunzip -c > $out/lib/firmware/facetimehd/firmware.bin || true
        '';
      });
    };
    */
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
    #listen = [ { ip = "*"; port = 8080;} ];
    #listen = [ { ip = "  127.0.0.1"; port = 8080;} ];
        #sslServerCert = "/tmp/gikos_net.cert";
        #sslServerKey = "/tmp/gikos_net.key";

    virtualHosts = {
      "silverspark.gikos.net" = {
        #listen = [ { ip = "  127.0.0.1"; port = 8080;} ];
        #listen = [ { ip = "  127.0.0.1"; port = 80;} ];
        documentRoot = "${spfiles}";
        extraConfig = ''
        
        <ifModule mod_headers.c>
          #Header set Access-Control-Allow-Origin 'origin-list'
          Header set Access-Control-Allow-Origin "http://localhost"
          Header always set Access-Control-Allow-Methods "POST, PUT, GET, DELETE, OPTIONS"
          Header always set Access-Control-Allow-Headers "Content-Type"           
        </ifModule>
        <Location /server-status>
            SetHandler server-status
            Require host localhost
            Order deny,allow
            Deny from all
            Allow from   127.0.0.0/255.0.0.0
        </Location>
        <Location />
            MellonEnable "info"
            MellonSPPrivateKeyFile ${spfiles}/private/localhost.key
            MellonSPCertFile ${spfiles}/public/localhost.cert
            MellonSpMetadataFile ${spfiles}/public/localhost.xml
            MellonIdPMetadataFile ${idpmetadata}
            MellonEndpointPath "/mellon"
        </Location>
        <Location /private>
            Options Indexes FollowSymLinks
            MellonEnable "auth"
            
        <IfModule mod_autoindex.c>
            Options Indexes FollowSymLinks
            IndexOptions FancyIndexing
            IndexOptions VersionSort
            IndexOptions HTMLTable
            IndexOptions FoldersFirst
            IndexOptions IconsAreLinks
            IndexOptions IgnoreCase
            IndexOptions SuppressDescription
            IndexOptions SuppressHTMLPreamble
            IndexOptions XHTML
            IndexOptions IconWidth=16
            IndexOptions IconHeight=16
            IndexOptions NameWidth=*
            IndexOrderDefault Descending Name
            HeaderName /index-style/header.html
            ReadmeName /index-style/footer.html
        </ifModule>
        </Location>

        <Location /basic_auth>
            AuthName "Please Log In"
            AuthType Basic
            require valid-user
            AuthUserFile ${basicPasswordFile}
        </Location>
        <Location /podview>
        </Location>
        <Location /hnix-frontend>
        </Location>

        '';
      };
      "hydra.gikos.net" = {
        documentRoot = "/var/lib/hydra/cache";

        extraConfig = ''
        
        <Location /private>
        <IfModule mod_autoindex.c>
            Options Indexes FollowSymLinks
            IndexOptions FancyIndexing
            IndexOptions VersionSort
            IndexOptions HTMLTable
            IndexOptions FoldersFirst
            IndexOptions IconsAreLinks
            IndexOptions IgnoreCase
            IndexOptions SuppressDescription
            IndexOptions SuppressHTMLPreamble
            IndexOptions XHTML
            IndexOptions IconWidth=16
            IndexOptions IconHeight=16
            IndexOptions NameWidth=*
            IndexOrderDefault Descending Name
            HeaderName /index-style/header.html
            ReadmeName /index-style/footer.html
        </ifModule>
        </Location>
        '';
      };
      "localhost2" = {
        documentRoot = "/home/kranium/git/github.com/haskell-nix/hnix-web-repl/result/ghcjs/hnix-frontend/bin/frontend.jsexe";
      };
    };
  };


  users.extraUsers.wwwrun.extraGroups = ["transmission" "hydra" ];

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



  #services.dockerRegistry.enable = true;
  environment.etc.hosts.mode = "0644";

  services.nfs.server = {
    enable     = true;
    exports    = ''
      /home/kranium/possplay/puppet-controlrepo *(rw,no_root_squash)
      /home/kranium/Downloads *
    '';
    statdPort  = 4000;
    lockdPort  = 4001;
    mountdPort = 4002;
  };

  # https://github.com/NixOS/nixpkgs/issues/14390 2/2
  environment.pathsToLink = [ "/share" ];

  environment.interactiveShellInit = ''
    # TERM=rxvt-unicode-256color seen in remote which makes backspace broken
    # use remove the 'unicode' part for now
    TERM=rxvt-256color
    # append history instead of overwrite
    shopt -s histappend
    # big history, record everything
    export HISTCONTROL=ignoredups:erasedups  # no duplicate entries
    export HISTSIZE=-1
    export HISTFILESIZE=-1
 '';

  services.xserver.displayManager.sessionCommands = ''
     # This allows GTK to load SVG icons.
    export GDK_PIXBUF_MODULE_FILE=$(echo ${pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/*/loaders.cache)
  '';


    programs.ssh.startAgent = true;

/*
    programs.ssh = {
      startAgent = true;
      knownHosts = [
        { hostNames = [ "10.20.60.129" ]; publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMpytQPiAi3AKnag8c78G/3XAmXkg+RltCYAnJnDQ2WY"; }     
        { hostNames = [ "10.20.60.2" ];   publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJaVuglSmptKElruSOG16xCFvCdT2WPp/mXGw2ReQVg/"; }
        { hostNames = [ "10.20.60.3" ];   publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILnYJyvxxVA6BsQYhZym+UgGPvtOWGmaHnwcFZiIvHLG"; }
        { hostNames = [ "10.20.60.4" ];   publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINUwrTVToDCcCqOPc3nAnagQQTq+B9BRNksPJmjiyvIG"; }
        { hostNames = [ "10.101.1.19" ];  publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINUwrTVToDCcCqOPc3nAnagQQTq+B9BRNksPJmjiyvIG"; }
      ];
    };
*/

  fonts = {
    enableDefaultFonts = false;
    enableFontDir = true;
    fonts = with pkgs; [
      dejavu_fonts
      emojione
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
      noto-fonts-emoji
      roboto
      roboto-mono
      roboto-slab
      source-code-pro
      source-sans-pro
      source-serif-pro
      inconsolata
      # nerdfonts # this is so big
    ];
    fontconfig = {
      #ultimate = {
      #  enable = false;
      #};
      defaultFonts = {
        monospace = [ "Source Code Pro" ];
        sansSerif = [ "Source Sans Pro" ];
        serif     = [ "Source Serif Pro" ];
      };
    };
  };

  services.influxdb.enable = true;

  #services.openldap.enable = true;
  #services.openldap.urlList = [ "ldapi:///" "ldap:///" ];

  services.softether.enable = true;
  services.softether.vpnclient.enable = true;

  services.mysql.enable = true;
  services.mysql.package = pkgs.mysql57;
  services.mysql.extraOptions = ''
    performance_schema = on
    innodb_strict_mode = off
  '';
  #services.mysql.package = pkgs.mariadb;

  #services.ntp.enable = true;

  #iphone mounting needs
  services.usbmuxd.enable = true;

  # services.minio.enable = true;
  # services.minio.listenAddress = ":12000";
  # services.minio.region = "ap-southeast-2";

  services.gnome3.gnome-keyring.enable = true;

  # services.etcd.enable = true;
  # services.flannel.enable = true;
  # services.flannel.network = "172.16.0.0/12"; #"10.10.0.0/24";

  boot.supportedFilesystems = [ "btrfs" "jfs" "reiserfs" "xfs" ];
  # system.nixos.stateVersion = "18.03"; # compat generation <= 1898
  system.stateVersion = "18.03";

  services.postgresql.enable = true;
  services.postgresql.enableTCPIP = true;
  services.postgresql.authentication = ''
    host all all 172.20.10.0/24 trust
    host all all 172.28.0.0/24 trust
    host all all 172.16.0.0/16 trust
    host all all 172.17.0.0/16 trust
  '';

  /*
  services.hydra.enable = true;
  services.hydra.hydraURL = "http://localhost:3000";
  services.hydra.notificationSender = "kranium@gikos.net";
  services.hydra.useSubstitutes = true;
  services.hydra.extraConfig = ''
    store_uri = file:///var/lib/hydra/cache?secret-key=/etc/nix/silverspark.gikos.net/secret
    binary_cache_secret_key_file = /etc/nix/silverspark.gikos.net/secret
    binary_cache_dir = /var/lib/hydra/cache
  '';

  */
  nix.trustedUsers = ["hydra" "hydra-evaluator" "hydra-queue-runner" "kranium" ];
  nix.distributedBuilds = true;
  nix.buildMachines = [
      /*
      {
        hostName = "localhost";
        systems = [ "x86_64-linux" "i686-linux" ];
        maxJobs = 6;
        # for building VirtualBox VMs as build artifacts, you might need other 
        # features depending on what you are doing
        supportedFeatures = [ ];
      }
      */
      # { hostName = "10.20.60.2"; systems = [ "armv7l-linux" ]; sshUser = "root"; sshKey = "/etc/nix/buildfarm"; maxJobs = 4; supportedFeatures = [ ]; }
      # { hostName = "10.20.60.3"; systems = [ "armv7l-linux" ]; sshUser = "root"; sshKey = "/etc/nix/buildfarm"; maxJobs = 4; supportedeatures = [ ]; }
      { hostName = "10.20.60.4"; systems = [ "armv7l-linux" ]; sshUser = "root"; sshKey = "/etc/nix/buildfarm"; maxJobs = 4; supportedeatures = [ ]; }
      # { hostName = "10.20.60.129"; systems = [ "armv7l-linux" ]; sshUser = "root"; sshKey = "/etc/nix/buildfarm"; maxJobs = 4; supportedFeatures = [ ]; }
      # { hostName = "10.101.1.19"; systems = [ "armv7l-linux" ]; sshUser = "root"; sshKey = "/etc/nix/buildfarm"; maxJobs = 4; supportedeatures = [ ]; }
    ];

  services.ddclient = {
    enable = true;
    use = "if, if=ens9";
    server = "dynamicdns.park-your-domain.com" ;
    username = secrets.ddclient.username;
    protocol = "namecheap";
    password = secrets.ddclient.password;
    extraConfig = secrets.ddclient.subdomain;
  };

  services.arbtt.enable = true;

  programs.tmux = {
    enable = true;
    historyLimit = 50000;
    extraTmuxConf = ''
      run-shell ${pkgs.tmuxPlugins.logging}/share/tmux-plugins/logging/logging.tmux
    '';
  };
}
