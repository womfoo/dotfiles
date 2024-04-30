{ pkgs, config, ... }:
{
  # this is bad practice but with zero load time
  # TODO: will move to per proj devshells + direnv
  environment.systemPackages = with pkgs; [
    # abiword
    acpi
    acpica-tools # was pmtools # acpidump
    # adoptopenjdk-jre-bin # java
    # aircrack-ng
    alsa-utils
    # ansible
    antimony
    apfs-fuse
    arandr
    # arc-theme
    # ardour
    arduino
    # aria2
    # arion
    # asciinema
    audacity
    # augeas
    avidemux
    awscli
    b2sum
    baobab
    bat
    # bcde # rip cds
    bettercap
    bind
    binutils # ld, ar
    bitcoin
    bluez-tools # bt-device --list
    bmon
    brave
    briss # crop pdf whitespace
    btrfs-progs
    # bundix # mini_portile2
    calibre
    # cargo
    carla
    # certbot
    cdrkit # wodim
    # cfssl
    # chromedriver
    # chromium
    cifs-utils
    # cli53
    colmena
    # compton-git
    conda
    config.boot.kernelPackages.bcc
    # config.boot.kernelPackages.bpftrace
    conntrack-tools
    #cpuminer-multi # deleted upstream
    cryptsetup
    cura
    # damon
    darcs
    darktable
    ddcutil
    # debootstrap
    d-spy # was dfeet and d-feet
    # diffoscope
    dillo
    direnv
    # discord
    dmenu
    dnsutils # nslookup
    docker-compose
    dos2unix
    dpkg # view files inside debs
    dropbox
    duc
    dvdplusrwtools
    # ec2_ami_tools
    # ec2_api_tools
    ecdsautils
    elfutils
    emacs
    emojipick
    encfs
    ethtool
    exfat
    facter
    f2fs-tools
    # fcitx # virtual keyboard
    # ffmpeg
    ffmpeg-full
    file
    find-cursor
    # firefox
    flac
    flameshot
    fnotifystat
    freecad
    forkstat
    # fpm
    fprintd
    # freecad
    fx
    fzf
    geoip
    gh
    ghostscript # needed by emacs doc-view
    gimp
    gitAndTools.hub
    gitFull
    glances
    glxinfo
    gnome.adwaita-icon-theme
    gnome.cheese
    gnome.eog
    gnome.evince
    gnome.file-roller
    gnome.nautilus
    gnome.seahorse # edit items in gnome-keyring
    # gnucash
    # gnumeric
    gnupg
    # gnupg1compat
    # gnutls
    # go
    # go-jira
    # go-mtpfs # jmtpfs and mtpfs fails on my xiaomi
    # go2nix
    google-chrome # /nix/store/pjq6jrm31xn2zw0cmwza3x0hg3lxpz2a-google-chrome-106.0.5249.119/bin/google-chrome-stable
    # google-chrome-beta
    # google-chrome-dev
    gpa
    gparted
    # gpodder
    gptfdisk
    gpxsee
    gradle
    # graphviz # dot
    grpcurl
    gsmartcontrol
    gst_all_1.gst-plugins-bad
    gst_all_1.gst-plugins-base
    gst_all_1.gst-plugins-good
    gst_all_1.gst-plugins-ugly
    gst_all_1.gstreamer
    gst_all_1.gst-devtools
    # gx
    # gx-go
    (haskellPackages.ghcWithPackages (
      self:
      with haskellPackages;
      with pkgs.haskell.lib;
      [
        network
        aeson
        aeson-qq
        # brick # 964 bork
        vty
        cursor
        lens
        xmobar
        dbus
        hpdft
        # skylighting
        # highlighting-kate
        # # arduino-copilot
        # pi-lcd # 964 bork
        # getopt-generics
        # stack
        # HPDF
        # HTF
        # HsOpenSSL
        # # bech32
        # brick
        # # brittany 9.2
        # cabal-install
        # cabal2nix
        # duckling
        # esqueleto
        # fast-logger
        # hlint
        # hnix
        # hnix-store-remote
        # http-conduit
        # mysql-simple
        # netlink
        # network-info
        # nix-derivation # pretty
        # nix-diff
        # nix-tree
        # password
        # postgresql-simple
        # # reanimate # fails on ghc 9.2 https://github.com/reanimate/reanimate/issues/299
        # semver-range
        # torrent
        # turtle
        # xmobar
        # xmonad
        # xmonad-contrib
        # # yeganesh
        # Monadoro
      ]
    ))
    # hfsprogs
    hicolor-icon-theme
    hiera-eyaml
    hledger
    hledger-web
    home-manager
    htop
    # httpie
    hwloc
    ifuse
    imagemagick
    imv
    inetutils
    # influxdb
    inkscape
    inotify-tools
    iotop
    ipcalc
    iperf
    iptraf-ng
    ispell
    iw
    # jmeter
    jq
    jmtpfs
    jwhois
    # k3b
    # kakoune-unwrapped
    kdiff3
    keepassxc
    # kitty
    # kops
    kotlin
    # kpcli
    # ktorrent
    # kubectl
    kubernetes-helm
    # languagetool # very-basic grammarly
    light
    libheif # make nautilus thumbnail HEIC, also requires pathToLink = share/thumbnailers
    libimobiledevice # idevice pair 00000000-0000000000000000
    libnotify # notify-send pp
    libphonenumber
    libreoffice
    librsvg # rsvg-convert
    # error: a 'i686-linux' with features {} is required to build '/nix/store/qaxgwyhh90dd0323sjn5qjdngvm4ai4z-nvidia-vaapi-driver-0.0.5.drv'
    #libva # vaapiVdpau should install this but I need vainfo
    # libva-utils
    libxml2 # xmllint
    libxslt
    lm_sensors
    lmdb # mdb_copy for backing up monero
    lsof
    macchanger
    magic-wormhole
    # masterpdfeditor
    mat2 # metadata anonymization
    mc
    meld
    minicom
    # minikube
    # monero
    # mosh
    # mplayer # (mplayer.override {v4lSupport =true;})
    mpv
    # msf
    mtools
    (mtr.override { withGtk = true; })
    ncdu
    # neovim
    net-snmp
    # netdata
    netdiscover
    nethogs
    networkmanagerapplet
    networkmanager_dmenu
    ngrep
    # niv
    nix-index
    nix-output-monitor
    nix-prefetch-git
    nix-top
    nixfmt-rfc-style
    # nixos-shell
    # nixops
    nmap
    # nmap-graphical
    nmon
    # node2nix
    nomacs
    ntfs3g
    # nur.repos.mic92.rhasspy #
    nvd # nix diff
    oathToolkit
    # obs-studio
    # okular
    # openjdk11_headless
    # openjdk17_headless
    openjdk19_headless
    # openjdk21_headless
    openldap # ldapsearch
    openssl
    # packer
    pandoc
    parallel
    parcellite
    parted
    pass
    pasystray
    patchutils # splitdiff
    patchelf
    pavucontrol
    peaclock
    pciutils # setpci
    pdfcrack
    # pdfmod
    pdftk
    # pdfstudio2023
    # pianobar
    pick
    picocom
    pipes # screensaver
    # pkgconfig
    # plasma5Packages.kdenlive
    poppler_utils # pdf2txt
    postgresql # just for the psql command
    # postman
    powerstat
    powertop
    ppp
    pptp
    procmail # lockfile
    protobuf # protoc
    psmisc # killall
    pssh
    pulsemixer
    pv
    # pypi2nix # unmaintained
    (python3.withPackages (
      ps:
      with ps;
      with python3Packages;
      [
        # (opencv4.override {enableGtk3 = true;})
        xattr
        opencv4
        gunicorn
        binwalk
        pip
        # python-lsp-server rope # broken 4-Aug-2022
        # epc importmagic # python emacs
        netifaces
        sqlparse
        # (pkgs.callPackage ../pkgs/metawear {}) # broken 28-Mar-2023
        # (pkgs.callPackage ../pkgs/xiaomi_mi_scale {})
        bluepy
        pylint
        pytest
      ]
    ))
    qemu
    # qpdf
    qrencode
    # r10k
    # ranger
    redshift
    # remmina # rdp
    rhash
    # ripgrep # ag for now until https://github.com/syl20bnr/spacemacs/issues/16200
    # rpm
    rpPPPoE
    # rrdtool
    rsync
    # rtl-sdr
    rtorrent
    tty-share
    # ruby # test if this fights with vagrant
    # runc
    rust-analyzer
    rustc
    rxvt-unicode
    # sbt
    # scala_3
    screen
    scrot
    shared-mime-info
    shellcheck
    # slack # resource hog
    signal-desktop
    silver-searcher # ag
    simplescreenrecorder
    # sipp
    # sipsak
    # slic3r
    smartmontools
    smemstat
    # sops
    # sox
    # spaceFM
    speechd # spd-say
    speedtest-cli
    # spotify
    sqlite
    sqlite.dev # because rails
    sqlitebrowser
    ssh-import-id
    ssh-to-pgp
    sshfs
    sshpass
    st
    # steam
    # subversionClient
    # surf
    sysstat # iotop, etc...
    tcpdump
    # teams
    # teamviewer
    # terminator
    # terraform # bitte does not support 1.x yet
    # terraform_0_13
    # tesseract
    texlive.combined.scheme-full
    thunderbird
    tig
    # timetrap
    tmate
    tmux
    tmux-cssh
    (tor-browser-bundle-bin.override { pulseaudioSupport = true; })
    trayer
    tree
    # tsung
    unzip
    usbutils # lsusb
    vagrant
    # vault
    vbetool # sudo vbetool dpms off
    vdpauinfo
    # veracrypt
    # victoriametrics
    vim
    # virtualbox # do not enable! virtualisation.virtualbox.host.enable = true is enough. weird erros occur.
    virt-manager
    virt-top
    # vlc
    # vulnix
    vscode
    vnstat
    # vscode
    wavemon # wifi diag
    wget
    which
    wire-desktop
    wirelesstools # iwconfig
    wireshark
    # wkhtmltopdf # vulnerable qtwebkit-5.212.0-alpha4
    # wxsqlite3
    # wxsqliteplus
    xawtv
    xcalib # calibrate colors
    xclip
    xdotool
    xfontsel
    xorg.xkill
    xorg.xwd
    xmlsec
    xournal
    xorg.xauth
    xorg.xev
    xorg.xdpyinfo
    xorg.xhost
    xorg.xlsfonts # font for xosd
    xorg.xwininfo
    # xosd
    xsane
    xscreensaver
    xzgv
    # yate
    yt-dlp # better youtube-dl
    yq
    z-lua
    zbar # parse qr codes
    # zfs
    zip
    # zoom-us
    # zsync
    # mobile-broadband-provider-info usb-modeswitch usb-modeswitch-data # TODO: will we need this?
  ];

  environment.pathsToLink = [ "share/thumbnailers" ];

}
