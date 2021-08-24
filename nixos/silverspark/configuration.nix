{ config, options, lib, pkgs, ... }:

let
  noplay = true; # no distractions
  # noplay = false;
  myfacter = pkgs.facter.override { libwhereami = null; };
  install1903Apps = false;
  newApps = with pkgs; [
    brave
    kdeApplications.konqueror
    tsung
  ];
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
      mkdir $out/test
      ln -s /home/kranium/git/github.com/haskell-nix/hnix-web-repl/result/ghcjs/hnix-frontend/bin/frontend.jsexe/index.html $out/test/
      ln -s /home/kranium/git/github.com/haskell-nix/hnix-web-repl/result/ghcjs/hnix-frontend/bin/frontend.jsexe/rts.js $out/test/
      ln -s /home/kranium/git/github.com/haskell-nix/hnix-web-repl/result/ghcjs/hnix-frontend/bin/frontend.jsexe/out.js $out/test/
      ln -s /home/kranium/git/github.com/haskell-nix/hnix-web-repl/result/ghcjs/hnix-frontend/bin/frontend.jsexe/lib.js $out/test/
      ln -s /home/kranium/git/github.com/haskell-nix/hnix-web-repl/result/ghcjs/hnix-frontend/bin/frontend.jsexe/runmain.js $out/test/
      ln -s /home/kranium/git/github.com/haskell-nix/hnix-web-repl/app/css $out/test/
      ln -s /home/kranium/git/github.com/haskell-nix/hnix-web-repl/app/js $out/test/
      mkdir $out/phonenumbers
      ln -s /home/kranium/git/github.com/google/libphonenumber/javascript/i18n/phonenumbers $out/phonenumbers
      mkdir $out/bullshit
    '';
  };
  secrets = import ./secrets.nix;
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./telegraf.nix
      ../shared/hydra.nix
      ../shared/gikos-kranium.nix
      # ./old-work.nix
      ../shared/fix-unstable-no-audio.nix
      #./asterisk-test.nix
    ];

  # boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernelPackages = pkgs.linuxPackages_5_12; # nvidia fails on 5.13

  # Use the gummiboot efi boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.timeout = 4;
  boot.loader.efi.canTouchEfiVariables = true;

  networking = {
    # nameservers = [ "8.8.8.8" "8.8.4.4" ];
    hostName = "silverspark";
    networkmanager = {
      enable = true;
      dns = "none";
    };
    firewall.logRefusedPackets = true;
    firewall.allowedTCPPorts = [
      22
      80                     # http
      111 # nfs? more
      2049 # nfs
      4000 # nfs/statd
      4001 # nfs/lockd
      4002 # nfs/mount
      5000 # FIXME: nix-serve temp move behind apache
      # 5060                   # sip
      # 5432                   # postgres
      8086 # telegraf
      # 8140 # puppet
      9200 # elastic
      7000 # FIXME: hydra temp move behind apache
    ];
    firewall.allowedUDPPorts = [
      53                       # dns
      67                       # udp client -> server (dhcp?)
      # 68                     # udp server ->
      111 # nfs? more
      2049 # nfs
      4000 # nfs/statd
      4001 # nfs/lockd
      4002 # nfs/mountd
      # 5060                   # sip
    ];
    firewall.allowedUDPPortRanges = [
      # https://blog.g3rt.nl/allow-google-chromecast-host-firewall-iptables.html
      { from = 32768; to = 61000; }
      # is this for asterisk?
      { from = 10000; to = 15000; }
    ];

    extraHosts = ''
      127.0.0.1 arawaraw
      127.0.0.1 silverspark.gikos.net
      127.0.0.1 geolite.maxmind.com
      10.61.164.26 ip.ipscape.local
      10.61.164.50 mulan.ipscape.local
      10.61.164.111 freeipa.ipscape.local
      10.61.164.155 pm.ipscape.local
      # 127.0.0.1 voipmonitor.org
      # 127.0.0.1 www.voipmonitor.org
      # 127.0.0.1 download.voipmonitor.org
      # 127.0.0.1 cloud.voipmonitor.org
      # 127.0.0.1 cloud2.voipmonitor.org
      # 127.0.0.1 cloud3.voipmonitor.org
      127.0.0.1 manager.gikos.net
      127.0.0.1 beamdocs
      127.0.0.1 miso
    '' +lib.optionalString noplay ''
      127.0.0.1 laarc.io
      127.0.0.1 lobste.rs
      127.0.0.1 news.ycombinator.com
      127.0.0.1 slashdot.org
      127.0.0.1 twitter.com
      127.0.0.1 www.youtube.com
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
      abcde # rip cds
      # abiword
      acpi
      ag
      aircrack-ng
      ansible
      antimony
      arandr
      arc-theme
      # arduino
      aria2
      asciinema
      augeas
      avidemux
      awscli
      baobab
      bettercap
      bind
      binutils # ld, ar
      # bitcoin
      bluez-tools # bt-device --list
      bmon
      btrfs-progs
      bundix
      calibre
      certbot
      cfssl
      # chromedriver
      # chromium
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
      # go-jira
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
      gst_all_1.gst-devtools
      # gx
      # gx-go
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
          mysql-simple
          nix-derivation # pretty
          password
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
      # ktorrent
      kubectl
      # languagetool # very-basic grammarly
      libnotify # notify-send pp
      libphonenumber
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
      ncdu
      neovim
      net_snmp
      # netdata
      nethogs
      networkmanagerapplet
      ngrep
      nix-index
      nix-output-monitor
      nix-prefetch-git
      nix-top
      nixfmt
      # nixops
      nmap
      nmap-graphical
      nomacs
      ntfs3g
      nur.repos.mic92.rhasspy
      oathToolkit
      okular
      openldap # ldapsearch
      openssl
      packer
      pandoc
      parallel
      parcellite
      parted
      pass
      pasystray
      patchelf
      pavucontrol
      pciutils # setpci
      pdfcrack
      pdfmod
      pdftk
      # pianobar
      pick
      picocom
      pipes # screensaver
      # pkgconfig
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
      # python36Packages.gunicorn
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
      # ruby_2_6
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
      sox
      spaceFM
      speedtest-cli
      # spotify
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
      # teamviewer
      terminator
      terraform
      # tesseract
      texlive.combined.scheme-full
      thunderbird
      tmux-cssh
      (tor-browser-bundle-bin.override { pulseaudioSupport = true;})
      trayer
      tree
      unzip
      usbutils # lsusb
      vagrant
      vdpauinfo
      # veracrypt
      vim
      # virtualbox # do not enable! virtualisation.virtualbox.host.enable = true is enough. weird erros occur.
      vlc
      # vulnix
      # vscode
      vnstat
      vscode
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

  services.xserver.screenSection = ''
    Option "metamodes" "nvidia-auto-select +0+0 { ForceFullCompositionPipeline = On }"
    Option         "AllowIndirectGLXProtocol" "off"
    Option         "TripleBuffer" "on"
  '';

  services.xserver.videoDrivers = [ "nvidia" ];       #non-free <-- faster but breaks ttys and brightness keys
  services.xserver.windowManager.xmonad.enable = true;                 # do not remove
  services.xserver.windowManager.xmonad.enableContribAndExtras = true; # do not remove
  services.xserver.displayManager.defaultSession = "none+xmonad";

  services.locate.enable = true;
  services.locate.pruneFS = [
    "afs"
    "anon_inodefs"
    "auto"
    "autofs"
    "bdev"
    "binfmt"
    "binfmt_misc"
    "cgroup"
    "cifs"
    "coda"
    "configfs"
    "cramfs"
    "cpuset"
    "debugfs"
    "devfs"
    "devpts"
    "devtmpfs"
    "ecryptfs"
    "eventpollfs"
    "exofs"
    "futexfs"
    "ftpfs"
    "fuse"
    "fusectl"
    "fuse.sshfs"
    "gfs"
    "gfs2"
    "hostfs"
    "hugetlbfs"
    "inotifyfs"
    "iso9660"
    "jffs2"
    "lustre"
    "misc"
    "mqueue"
    "ncpfs"
    "nnpfs"
    "ocfs"
    "ocfs2"
    "pipefs"
    "proc"
    "ramfs"
    "rpc_pipefs"
    "securityfs"
    "selinuxfs"
    "sfs"
    "shfs"
    "smbfs"
    "sockfs"
    "spufs"
    # "nfs"
    # "NFS"
    # "nfs4"
    # "nfsd"
    "sshfs"
    "subfs"
    "supermount"
    "sysfs"
    "tmpfs"
    "ubifs"
    "udf"
    "usbfs"
    "vboxsf"
    "vperfctrfs"
  ];

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
  hardware.opengl.extraPackages = with pkgs; [ libvdpau-va-gl vaapiVdpau ];

  time.timeZone = "Australia/Sydney";

  virtualisation.docker.enable = true;

  services.flatpak.enable = true;
  xdg.portal.enable = true;
  virtualisation.virtualbox.host.enable = true;
  virtualisation.libvirtd.enable = true;

  nixpkgs.config = {
    allowBroken = true;
    allowUnfree = true;
    overlays = [ "/home/kranium/git/github.com/stesie/azure-cli-nix" ];
    packageOverrides = pkgs: {
      nur = import /home/kranium/git/github.com/nix-community/nur-combined {
        inherit pkgs;
      };
      haskellPackages = pkgs.haskellPackages.override {
            overrides = hsSelf: hsSuper: {
              sproxy2 = hsSelf.callPackage /home/kranium/git/github.com/ip1981/sproxy2/default.nix { } ;
              docopt = hsSelf.callPackage /home/kranium/git/github.com/jefdaj/docopt.hs/default.nix { };
            };
      };
    };
    # permittedInsecurePackages = [
    #   "openssl-1.0.2u"
    # ];
  };

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
      "beamdocs" = {
        documentRoot = "/home/kranium/git/github.com/haskell-beam/beam/site";
      };
      "miso.gikos.net" = {
        documentRoot = "/home/kranium/git/github.com/dmjio/miso";
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
    # "https://thefloweringash-armv7.cachix.org"
    # "https://nixcache.reflex-frp.org"
    # "https://static-haskell-nix.cachix.org"
    # "https://hydra.iohk.io"
    # "https://miso-haskell.cachix.org"
  ];
  nix.binaryCachePublicKeys = [
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    # "thefloweringash-armv7.cachix.org-1:v+5yzBD2odFKeXbmC+OPWVqx4WVoIVO6UXgnSAWFtso="
    # "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
    # "static-haskell-nix.cachix.org-1:Q17HawmAwaM1/BfIxaEDKAxwTOyRVhPG5Ji9K3+FvUU="
    # "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    # "miso-haskell.cachix.org-1:6N2DooyFlZOHUfJtAx1Q09H0P5XXYzoxxQYiwn6W1e8="
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
      /home/kranium/Downloads *(ro,no_root_squash,no_subtree_check,fsid=1)
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

  services.influxdb.enable = true;

  #iphone mounting needs
  services.usbmuxd.enable = true;

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

  nix.trustedUsers = ["hydra" "hydra-evaluator" "hydra-queue-runner" "kranium" ];

  nix.extraOptions = ''
    keep-outputs = true
    extra-platforms = aarch64-linux
  '';

  services.arbtt.enable = true;
  # services.arbtt.package = pkgs.haskell.packages.ghc8104.arbtt;

  environment.variables = {
      PATH = "$PATH:/home/kranium/bin";
  };

  programs.gnupg.agent.enable = true;

  programs.tmux = {
    enable = true;
    historyLimit = 50000;
    extraConfig = ''
      run-shell ${pkgs.tmuxPlugins.logging}/share/tmux-plugins/logging/logging.tmux
    '';
  };

  services.grafana = {
    enable = true;
  };

  services.paperless.enable = true;
  services.paperless.address = "0.0.0.0";
  services.paperless.extraConfig = {
    PAPERLESS_TIME_ZONE = "Australia/Sydney";
    PAPERLESS_DISABLE_LOGIN = "false";
    PAPERLESS_LIST_PER_PAGE = 1000;
    PAPERLESS_ALLOWED_HOSTS = "paperless.kranium.net,127.0.0.1";
    #PAPERLESS_INLINE_DOC=  "false";
  };

}
