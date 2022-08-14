{ pkgs, config, ... }:
{
  # this is bad practice but with zero load time
  # TODO: will move to per proj devshells + direnv
  environment.systemPackages = with pkgs;
    [
      # abcde # rip cds
      # abiword
      acpi
      acpica-tools # was pmtools # acpidump
      # adoptopenjdk-jre-bin # java
      aircrack-ng
      alsaUtils
      ansible
      antimony
      arandr
      # arc-theme
      ardour
      # arduino
      aria2
      arion
      asciinema
      audacity
      augeas
      avidemux
      awscli
      baobab
      bat
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
      cargo
      carla
      certbot
      cdrkit # wodim
      cfssl
      # chromedriver
      # chromium
      cifs-utils
      cli53
      colmena
      # compton-git
      config.boot.kernelPackages.bcc
      # config.boot.kernelPackages.bpftrace
      conntrack-tools
      #cpuminer-multi # deleted upstream
      cryptsetup
      cura
      damon
      darcs
      darktable
      # debootstrap
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
      ec2_ami_tools
      ec2_api_tools
      ecdsautils
      elfutils
      emacs
      encfs
      ethtool
      exfat
      facter
      f2fs-tools
      # fcitx # virtual keyboard
      ffmpeg
      file
      # firefox
      flac
      flameshot
      fnotifystat
      forkstat
      # fpm
      # freecad
      fzf
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
      gnome3.seahorse # edit items in gnome-keyring
      # gnucash
      # gnumeric
      gnupg
      # gnupg1compat
      # gnutls
      go
      # go-jira
      # go-mtpfs # jmtpfs and mtpfs fails on my xiaomi
      # go2nix
      google-chrome
      # google-chrome-beta
      # google-chrome-dev
      gpa
      gparted
      # gpodder
      gptfdisk
      gpxsee
      graphviz
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
      (haskellPackages.ghcWithPackages (self:
        with haskellPackages;
        with pkgs.haskell.lib; [
          HPDF
          HTF
          HsOpenSSL
          brick
          brittany
          cabal-install
          cabal2nix
          # duckling
          esqueleto
          fast-logger
          hlint
          hnix
          hnix-store-remote
          http-conduit
          mysql-simple
          netlink
          network-info
          nix-derivation # pretty
          nix-diff
          password
          postgresql-simple
          reanimate
          semver-range
          torrent
          turtle
          xmobar
          xmonad
          xmonad-contrib
          # yeganesh
        ]))
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
      # jmeter
      jq
      jwhois
      # k3b
      kakoune-unwrapped
      kdiff3
      keepassx
      # kitty
      # kops
      kotlin
      # kpcli
      # ktorrent
      # kubectl
      # kubernetes-helm
      # languagetool # very-basic grammarly
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
      masterpdfeditor
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
      # nixos-shell
      # nixops
      nmap
      # nmap-graphical
      nmon
      node2nix
      nomacs
      ntfs3g
      # nur.repos.mic92.rhasspy #
      nvd
      oathToolkit
      # obs-studio
      # okular
      openjdk11_headless
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
      pciutils # setpci
      pdfcrack
      # pdfmod
      pdftk
      # pianobar
      pick
      picocom
      pipes # screensaver
      # pkgconfig
      # plasma5Packages.kdenlive
      poppler_utils # pdf2txt
      postgresql #just for the psql command
      # postman
      powerstat
      ppp
      pptp
      procmail # lockfile
      psmisc # killall
      pssh
      pulsemixer
      pv
      pypi2nix
      (python3.withPackages (
        ps: with ps; with python3Packages; [
          gunicorn
          binwalk
          pip
          # python-lsp-server rope # broken 4-Aug-2022
          # epc importmagic # python emacs
          netifaces
          sqlparse
          (pkgs.callPackage ../pkgs/metawear {})
          (pkgs.callPackage ../pkgs/xiaomi_mi_scale {})
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
      remmina # rdp
      rhash
      ripgrep
      # rpm
      # rrdtool
      rsync
      # rtl-sdr
      rtorrent
      tty-share
      ruby
      # runc
      rust-analyzer
      rustc
      rxvt_unicode-with-plugins
      sbt
      scala
      screen
      scrot
      shared-mime-info
      shellcheck
      # slack # resource hog
      signal-desktop
      silver-searcher # ag
      # simplescreenrecorder
      # sipp
      # sipsak
      smartmontools
      smemstat
      # sops
      # sox
      # spaceFM
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
      sysstat # iotop, etc...
      tcpdump
      # teams
      # teamviewer
      # terminator
      # terraform # bitte does not support 1.x yet
      # terraform_0_13
      # tesseract
      texlive.combined.scheme-full
      # thunderbird
      tig
      tmate
      tmux
      tmux-cssh
      (tor-browser-bundle-bin.override { pulseaudioSupport = true;})
      trayer
      tree
      # tsung
      unzip
      usbutils # lsusb
      vagrant
      vdpauinfo
      # veracrypt
      vim
      # virtualbox # do not enable! virtualisation.virtualbox.host.enable = true is enough. weird erros occur.
      virt-manager
      # vlc
      # vulnix
      vscode
      vnstat
      # vscode
      wavemon
      wget
      which
      wire-desktop
      wirelesstools # iwconfig
      wireshark
      wkhtmltopdf
      wxsqlite3
      wxsqliteplus
      xawtv
      xcalib # calibrate colors
      xclip
      xdotool
      xfontsel
      xorg.xkill
      xorg.xwd
      xmlsec
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
      zfs
      zip
      zoom-us
      # zsync
    ];

  hardware.enableAllFirmware = true;

}
