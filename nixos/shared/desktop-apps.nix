{ pkgs, config, ... }: {

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
      brave
      btrfs-progs
      # bundix # mini_portile2
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
      # facter
      f2fs-tools
      # fcitx # virtual keyboard
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
      # kdiff3-qt5
      keepassx
      # kitty
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
      # monero
      mosh
      mpv
      # msf
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
          sqlparse
          (pkgs.callPackage ../pkgs/metawear {})
        ]
      ))
      qemu
      qpdf
      # r10k
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
      # spaceFM
      speedtest-cli
      # spotify
      sqlite
      sqlitebrowser
      sshfs
      sshpass
      st
      # steam
      # subversionClient
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
      tsung
      unzip
      usbutils # lsusb
      # vagrant
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
    ];
}
