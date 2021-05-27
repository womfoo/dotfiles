# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, options, pkgs, ... }:

let
  myfacter = pkgs.facter.override { libwhereami = null; };
  serverIP = "0.0.0.0";
  # serverIP = "10.101.11.82";
  # serverIP = "10.101.11.82";
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

  wire-server = pkgs.haskellPackages.callPackage /home/kranium/git/github.com/wireapp/wire-server { };
  myAMI = pkgs.haskellPackages.callPackage /home/kranium/AMI-0.1/default.nix { };
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

  boot.kernelPackages = pkgs.linuxPackages_latest;

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
      8140 # puppet lol
      9200 # elastic
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
  environment.systemPackages = with pkgs;
    [
      abcde
      abiword
      acpi
      ag
      aircrack-ng
      ansible
      antimony
      arandr
      arc-theme
      arduino
      aria2
      asciinema
      augeas
      avidemux
      awscli
      azure-cli
      azure-storage-azcopy
      baobab
      bettercap
      bind
      binutils # ld, ar
      # bitcoin
      bluez-tools # bt-device --list
      bmon
      btrfs-progs
      bundix
      certbot
      cfssl
      chromedriver
      chromium
      cifs_utils
      cli53
      compton-git
      config.boot.kernelPackages.bcc
      config.boot.kernelPackages.bpftrace
      conntrack_tools
      cpuminer-multi
      cryptsetup
      cura
      darcs
      darktable
      debootstrap
      dillo
      discord
      dmenu
      dnsutils # nslookup
      docker_compose
      dos2unix
      dpkg # view files inside debs
      dropbox
      duc
      ec2_ami_tools
      ec2_api_tools
      ecdsautils
      elfutils
      emacs
      encfs
      exfat
      exfat-utils
      # facter
      myfacter
      f2fs-tools
      fcitx
      ffmpeg
      file
      firefox
      flac
      flameshot
      fnotifystat
      forkstat
      fpm
      freecad
      fuse_exfat
      geoip
      ghostscript # needed by emacs doc-view
      gimp
      gitAndTools.hub
      gitFull
      glxinfo
      gnome3.adwaita-icon-theme
      gnome3.cheese
      gnome3.evince
      gnome3.file-roller
      gnome3.librsvg
      gnome3.seahorse # edit items in gnome-keyring
      # gnucash
      # gnumeric
      # gnupg1compat
      # gnutls
      go
      go-jira
      go-mtpfs # jmtpfs and mtpfs fails on my xiaomi
      go2nix
      google-chrome
      # google-chrome-beta
      google-chrome-dev
      gpa
      gparted
      gpodder
      gptfdisk
      gpxsee
      graphviz
      gsmartcontrol
      gst_all_1.gst-plugins-bad
      gst_all_1.gst-plugins-base
      gst_all_1.gst-plugins-good
      gst_all_1.gst-plugins-ugly
      gst_all_1.gstreamer
      gx
      gx-go
      (haskellPackages.ghcWithPackages (self:
        with haskellPackages;
        with pkgs.haskell.lib; [
          HsOpenSSL
          cabal-install
          cabal2nix
          esqueleto
          fast-logger
          hlint
          http-conduit
          myAMI
          mysql-simple
          nix-derivation # pretty
          postgresql-simple
          semver-range
          torrent
          turtle
          xmobar
          xmonad
          xmonad-contrib
          yeganesh
          brick
        ]))
      hfsprogs
      hicolor-icon-theme
      hiera-eyaml
      hledger
      hledger-web
      htop
      httpie
      hwloc
      ifuse
      imagemagick
      inetutils
      influxdb
      inkscape
      inotifyTools
      iotop
      ipcalc
      iperf
      iptraf-ng
      ispell
      iw
      jmeter
      jq
      jwhois
      kdiff3-qt5
      keepassx
      kitty
      kops
      kpcli
      ktorrent
      kubectl
      languagetool
      libnotify # notify-send pp
      libphonenumber
      librarian-puppet-go
      libreoffice
      libva-full # vaapiVdpau should install this but I need vainfo
      libva-utils
      libxml2 # xmllint
      libxslt
      lm_sensors
      lmdb # mdb_copy for backing up monero
      lsof
      masterpdfeditor
      mc
      meld
      minicom
      monero
      mosh
      mpv
      msf
      (mtr.override { withGtk = true; })
      mysql
      ncdu
      neovim
      net_snmp
      netdata
      nethogs
      networkmanager_l2tp
      networkmanager_openconnect
      networkmanagerapplet
      ngrep
      nix-index
      nix-prefetch-git
      nix-top
      nixfmt
      nmap
      nmap-graphical
      nomacs
      ntfs3g
      oathToolkit
      okular
      openconnect_openssl
      openldap # ldapsearch
      openssl
      packer
      pandoc
      parallel
      parcellite
      pass
      pasystray
      patchelf
      pavucontrol
      pciutils # setpci
      pdfcrack
      pdfmod
      pdftk
      pianobar
      pick
      picocom
      pipes # screensaver
      pkgconfig
      pmtools # acpidump
      poppler_utils # pdf2txt
      postgresql #just for the psql command
      postman
      powerstat
      ppp
      pptp
      procmail # lockfile
      psmisc # killall
      pssh
      pulsemixer
      pv
      pypi2nix
      python3
      python36Packages.gunicorn
      python3Packages.binwalk
      python3Packages.pip
      python3Packages.sqlparse
      qemu
      qpdf
      r10k
      ranger
      redshift
      remmina # rdp
      rhash
      rpm
      rrdtool
      rsync
      rtl-sdr
      ruby_2_6
      runc
      rxvt_unicode-with-plugins
      screen
      scrot
      shared_mime_info
      shellcheck
      # slack # resource hog
      signal-desktop
      simplescreenrecorder
      sipp
      sipsak
      smartmontools
      smemstat
      softether
      sox
      spaceFM
      speedtest-cli
      spotify
      sqlite
      sqlitebrowser
      sshfs
      sshpass
      st
      # steam
      subversionClient
      sysstat # iotop, etc...
      tcpdump
      # teams
      teamviewer
      terminator
      terraform
      # tesseract
      texlive.combined.scheme-full
      thunderbird
      tmux-cssh
      torbrowser
      trayer
      tree
      unzip
      usbutils # lsusb
      vagrant
      vdpauinfo
      veracrypt
      vim
      # virtualbox # do not enable! virtualisation.virtualbox.host.enable = true is enough. weird erros occur.
      vlc
      # vulnix
      # vscode
      vnstat
      wavemon
      wget
      which
      wire-desktop
      wirelesstools # iwconfig
      wireshark
      wkhtmltopdf
      wxsqlite3
      wxsqliteplus
      xcalib # calibrate colors
      xclip
      xdotool
      xfontsel
      xlibs.xkill
      xlibs.xwd
      xmlsec
      xorg.xauth
      xorg.xdpyinfo
      xorg.xhost
      xorg.xlsfonts # font for xosd
      xorg.xwininfo
      xosd
      xsane
      xscreensaver
      xzgv
      yate
      youtubeDL
      yq
      zbar # parse qr codes
      zfs
      zip
      zoom-us
      zsync
    ] ++ lib.optional install1903Apps newApps;

  #environment.etc = {
  #  "libao.conf".text = ''
  #    default_driver=pulse
  #  '';
  #};

  services.tftpd.enable = true;
  services.dovecot2.enable = true;

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
  services.xserver.displayManager.defaultSession = "none+xmonad";

  services.locate.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.kranium = {
     name = "kranium";
     extraGroups = [ "wheel" "networkmanager" "audio" "docker" "vboxusers" "video" "lp" "dialout" "libvirtd" "kranium"];
     group = "users";
     uid = 2000;
     createHome = true;
     home = "/home/kranium";
     shell = "/run/current-system/sw/bin/bash";
     isNormalUser = true;
  };
  users.extraGroups = { networkmanager = { } ; kranium = { gid = 2000; } ; telegraf = { }; } ;

  users.extraUsers.telegraf = {
    extraGroups = [ "telegraf" ];
  };
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
      "arawaraw" = {
        documentRoot = "/home/kranium/arawaraw";
      };
      "localhost2" = {
        documentRoot = "/home/kranium/git/github.com/haskell-nix/hnix-web-repl/result/ghcjs/hnix-frontend/bin/frontend.jsexe";
      };
      "geolite.maxmind.com" = {
        documentRoot = "/home/kranium/geoip";
      };
      "mkdocs" = {
        documentRoot = "/home/kranium/work/gits/ops/site";
      };
      "beamdocs" = {
        documentRoot = "/home/kranium/git/github.com/haskell-beam/beam/site";
      };
    };
  };


  users.extraUsers.wwwrun.extraGroups = ["transmission" "hydra" ];

  # programs.java.enable = true;

  programs.light.enable = true;
  programs.kbdlight.enable = true;

  nix.useSandbox = true;
  nix.buildCores = 4;

  nix.binaryCaches = [
    "https://cache.nixos.org/"
    "https://nixcache.reflex-frp.org"
    "https://static-haskell-nix.cachix.org"
    "https://hydra.iohk.io"
  ];
  nix.binaryCachePublicKeys = [
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
    "static-haskell-nix.cachix.org-1:Q17HawmAwaM1/BfIxaEDKAxwTOyRVhPG5Ji9K3+FvUU="
    "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
  ];
  # https://nixos.wiki/wiki/Overlays
  nix.nixPath = options.nix.nixPath.default ++
  [ "nixpkgs-overlays=/etc/nixos/overlays-compat/" ]
  ;



  #services.dockerRegistry.enable = true;
  environment.etc.hosts.mode = "0644";

  services.nfs.server = {
    enable     = true;
    createMountPoints = true;
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
      font-awesome
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
  services.mysql.settings = {
    mysqld = {
      performance_schema = "on";
      innodb_strict_mode = false;
    };
  };

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


  # docker-containers.netbox = {
  #   image = "netboxcommunity/netbox";
  # };
  # networking.networkmanager.dns = "none";

  # deprecated
  # docker-containers.pihole = {
  virtualisation.oci-containers.containers.pihole = {
    # autoStart = false; # display-manager delay?
    image = "pihole/pihole:latest";
    ports = [
      "${serverIP}:53:53/tcp"
      "${serverIP}:53:53/udp"
      "3080:80"
      "30443:443"
    ];
    volumes = [
      "/var/lib/pihole/:/etc/pihole/"
      "/var/lib/dnsmasq/.d:/etc/dnsmasq.d/"
    ];
    environment = {
      ServerIP = serverIP;
    };
    #extraDockerOptions = [
    extraOptions = [
      "--cap-add=NET_ADMIN"
      "--dns=127.0.0.1"
      "--dns=1.1.1.1"
    ];
    workdir = "/var/lib/pihole/";
  };
  networking.networkmanager.dns = "none";

  services.grafana = {
    enable = true;
  };
/*
  services.dnsmasq = {
    enable = true;
    servers = [ "8.8.8.8" "8.8.4.4" ];
    extraConfig = ''
     domain=kraniumlan
      interface=eth0
      bind-interfaces
      dhcp-option=6,8.8.8.8,8.8.4.4
      dhcp-range=10.20.60.2,10.20.60.200,24h
    '';
  };
  networking.firewall.allowPing = true;
  networking.interfaces = { enp0s20u1u2 = { ipv4 = { addresses = [ { address = "10.20.60.1"; prefixLength = 24; } ] ; } ; } ; };
*/

  systemd.user.services.ff-backup = {
    description = "backup firefox profile when logging in";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    path = [ pkgs.bzip2 pkgs.gnutar pkgs.coreutils];

    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${pkgs.bash}/bin/bash -c 'tar cjvf ~/backup/mozilla-backup-''$(date +%%Y%%m%%d_%%H%%M%%S\).tar.bz2 ~/.mozilla --ignore-failed-read'";
      # ExecStart = "${pkgs.firefox}/bin/firefox";
    };
  };
/*
  services.paperless.enable = true;
  services.paperless.address = "0.0.0.0";
  services.paperless.extraConfig = {
    PAPERLESS_TIME_ZONE = "Australia/Sydney";
    PAPERLESS_DISABLE_LOGIN = "false";
    PAPERLESS_LIST_PER_PAGE = 1000;
    PAPERLESS_ALLOWED_HOSTS = "paperless.kranium.net,127.0.0.1";
    #PAPERLESS_INLINE_DOC=  "false";
  };
*/
  /* hack to make shell less greener for stupid themes */
  /*
  programs.bash.promptInit = ''
    # Provide a nice prompt if the terminal supports it.
    if [ "$TERM" != "dumb" -o -n "$INSIDE_EMACS" ]; then
      PROMPT_COLOR="1;31m"
      let $UID && PROMPT_COLOR="0;32m"
      if [ -n "$INSIDE_EMACS" -o "$TERM" == "eterm" -o "$TERM" == "eterm-color" ]; then
        # Emacs term mode doesn't support xterm title escape sequence (\e]0;)
        PS1="\n\[\033[$PROMPT_COLOR\][\u@\h:\w]\\$\[\033[0m\] "
      else
        PS1="\n\[\033[$PROMPT_COLOR\][\[\e]0;\u@\h: \w\a\]\u@\h:\w]\\$\[\033[0m\] "
      fi
      if test "$TERM" = "xterm"; then
        PS1="\[\033]2;\h:\u:\w\007\]$PS1"
      fi
    fi
  '';
  */
  services.haproxy.enable = true;
  services.haproxy.config = ''
    defaults
      log  global
      maxconn  6000
      mode  http
      option  redispatch
      option  dontlognull
      option  http-server-close
      option  abortonclose
      option  splice-auto
      timeout  connect 10s
      timeout  client 10m
      timeout  server 10m
      timeout  queue 10s
      timeout  http-keep-alive 3s

    listen puppet
      bind 0.0.0.0:8140
      mode tcp
      server vag1-pm-0001 172.20.10.4:8140 check
      server vag1-pm-0002 192.168.1.146:8140 check
    listen stats_external
      bind *:1600
      mode http
      bind-process 1
      stats enable
      stats uri /haproxy
      stats auth haproxy:P@ssw0rd
      stats admin if TRUE
  '';
  services.vector = {
    enable = true;
    journaldAccess = true;
    settings = {
      sources.xin  = {
        type = "journald";
      };
      sinks.outs = {
        inputs  = ["in"];
        type    = "elasticsearch";
        host    = "http://127.0.0.1:9200";
      };
    };
  };
  services.elasticsearch.enable = true;
  services.kibana.enable = true;

}
