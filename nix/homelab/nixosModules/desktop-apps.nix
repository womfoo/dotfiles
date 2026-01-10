{
  pkgs,
  ...
}:
{
  # this is bad practice but with zero load time
  # TODO: will move to per proj devshells + direnv
  environment.systemPackages =
    with inputs.cells.vendor.packages;
    with inputs.cells.desktop.packages;
    with pkgs;
    [
      # abiword
      acpi
      acpica-tools # was pmtools # acpidump
      #2405#adwaita-icon-theme
      # adoptopenjdk-jre-bin # java
      # aircrack-ng
      alsa-utils
      # ansible
      ansible-22-05
      # antimony # ftbs 20251015
      apfs-fuse
      arandr
      # arc-theme
      # ardour
      arduino
      # aria2
      # arion
      # asciinema
      (aspellWithDicts (
        dicts: with dicts; [
          en
          en-computers
          en-science
          tl
        ]
      ))
      audacity
      ansible
      # augeas
      awscli
      b2sum
      b3sum
      baobab
      bat
      # bcde # rip cds
      bettercap
      bind
      binutils # ld, ar
      # bitcoin
      blueberry
      bluez-tools # bt-device --list
      # bluetooth_battery
      bmon
      brave
      # briss # crop pdf whitespace
      btrfs-progs
      # bundix # mini_portile2
      # calibre # fixme
      # cargo
      # carla # 20250705 cython-0.29.37.1 not supported for interpreter python3.13
      # certbot
      cdrkit # wodim
      # cfssl
      # cheese
      # chromedriver
      # chromium
      cifs-utils
      # cli53
      # colmena
      # compton-git
      conda
      config.boot.kernelPackages.bcc
      # config.boot.kernelPackages.bpftrace
      conntrack-tools
      #cpuminer-multi # deleted upstream
      cryptsetup
      # cura # /nix/store/819gmnvr4j21zc2jpyvij4q1ch6p11nx-cura-4.13.1/bin/cura
      # damon
      darcs
      darktable-25-05
      dbeaver-bin
      ddcutil
      # debootstrap
      d-spy # was dfeet and d-feet
      # diffoscope
      dillo
      # discord
      dive
      dmenu
      dnsutils # nslookup
      docker-compose
      dos2unix
      dpkg # view files inside debs
      duc
      dvdplusrwtools
      # ec2_ami_tools
      # ec2_api_tools
      ecdsautils
      elfutils
      emacs
      emojipick
      # eog
      encfs
      ethtool
      evince
      evtest
      exfat
      exfatprogs
      facter
      f2fs-tools
      f3
      # fcitx # virtual keyboard
      # ffmpeg
      # ffmpeg-full # fixme
      file
      # file-roller
      find-cursor
      # firefox
      flac
      flameshot
      fnotifystat
      # freecad
      forkstat
      # fpm
      # fprintd # fingerprint daemon, build timeouts
      # freecad
      fx
      fzf
      geoip
      gemini-cli
      gh
      # ghidra
      ghostscript # needed by emacs doc-view
      gimp
      # gitAndTools.hub
      # gitFull
      glances
      # gnucash
      # gnumeric
      gnupg
      # gnupg1compat
      # gnutls
      # go
      # go-jira
      # go-mtpfs # jmtpfs and mtpfs fails on my xiaomi
      # go2nix
      # google-chrome-beta
      # google-chrome-dev
      gpa
      gparted
      # gpodder
      gptfdisk
      gpustat
      gpxsee
      # gradle
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
          # brick # ftbs 20251015
          # brick-tabular-list # ftbs 20251015
          cabal2nix
          cursor
          dbus
          # hpdft
          # hw-ip # ftbs 20251015
          iproute
          lens
          network
          network-info
          timeline
          vector
          vty
          zim-parser
        ]
      ))
      # hfsprogs
      hicolor-icon-theme
      # hiera-eyaml
      hledger
      hledger-web
      home-manager
      htop
      httrack
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
      # jetbrains.idea-community
      # jmeter
      jq
      jmtpfs
      # jwhois
      # k3b
      k6
      # kakoune-unwrapped
      kdiff3
      keepassx-22-11
      keepassxc
      # kitty
      kiwix-tools
      # kops
      # kotlin
      # kpcli
      # ktorrent
      # kubectl
      kubernetes-helm
      # languagetool # very-basic grammarly
      lefthook
      light
      libgpiod
      libheif # make nautilus thumbnail HEIC, also requires pathToLink = share/thumbnailers
      libimobiledevice # idevice pair 00000000-0000000000000000
      libnotify # notify-send pp
      libphonenumber
      libreoffice
      librsvg # rsvg-convert
      # error: a 'i686-linux' with features {} is required to build '/nix/store/qaxgwyhh90dd0323sjn5qjdngvm4ai4z-nvidia-vaapi-driver-0.0.5.drv'
      #libva # vaapiVdpau should install this but I need vainfo
      libva-utils
      libxml2 # xmllint
      libxslt
      linssid
      lm_sensors
      lmdb # mdb_copy for backing up monero
      lnav
      lsof
      macchanger
      # magic-wormhole
      # masterpdfeditor
      mat2 # metadata anonymization
      mc
      meld
      mesa-demos
      # metals # scala language server
      minicom
      # minikube
      # monero
      # mosh
      # mplayer # (mplayer.override {v4lSupport =true;})
      # mpv
      # msf
      mtools
      (mtr.override { withGtk = true; })
      # nautilus
      ncdu
      # neovim
      net-snmp
      net-tools
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
      # nomacs
      ntfs3g
      # nur.repos.mic92.rhasspy #
      nvd # nix diff
      oath-toolkit
      # obs-studio
      # okular
      # openjdk11_headless
      # openjdk17_headless
      # openjdk19_headless
      # openjdk21_headless
      openldap # ldapsearch
      openssl
      # packer
      pandoc
      parallel
      parcellite-25-11
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
      playerctl
      poppler-utils # pdf2txt
      postgresql # just for the psql command
      # postman
      powerstat
      powertop
      # ppp
      # pptp
      # procmail # lockfile
      protobuf # protoc
      psmisc # killall
      # pssh
      pulsemixer
      pv
      pwgen
      # pypi2nix # unmaintained
      # (python3.withPackages (
      #   ps:
      #   with ps;
      #   with python3Packages;
      #   [
      #     binwalk
      #     bluepy
      #     boto3
      #     dbus
      #     kubernetes
      #     # (pkgs.callPackage ../pkgs/metawear {}) # broken 28-Mar-2023
      #     netifaces
      #     # (opencv4.override {enableGtk3 = true;})
      #     opencv4
      #     pylint
      #     pytz
      #     # (pkgs.callPackage ../pkgs/xiaomi_mi_scale {})
      #   ]
      # ))
      # qemu
      # qpdf
      qrencode
      # r10k
      # ranger
      # redshift
      remmina # rdp
      rhash
      ripgrep # ag for now until https://github.com/syl20bnr/spacemacs/issues/16200
      rmlint
      # rpm
      # rpPPPoE
      # rrdtool
      rsync
      # rtl-sdr
      # rtorrent
      # tty-share
      # ruby # test if this fights with vagrant
      # runc
      # rust-analyzer
      # rustc
      # rxvt-unicode # urxvt
      # (sbt.override { jre = jdk17; })
      sarif-tools
      # scala_3
      # seahorse # edit items in gnome-keyring
      screen
      scrot
      semgrep
      shared-mime-info
      # shellcheck
      # slack # resource hog
      signal-desktop
      # silver-searcher # ag
      simple-scan
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
      # sqlite.dev # because rails
      sqlitebrowser
      ssh-import-id
      ssh-to-pgp
      sshfs
      # sshpass
      st
      # steam
      # step-cli # FIXME
      # step-ca
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
      # texlive.combined.scheme-full

      thunderbird
      tig
      # timetrap
      tmate
      tmux
      # tmux-cssh
      trayer
      tree
      # tsung
      # uucp
      unzip
      usbutils # lsusb
      v4l-utils # ir-keytable
      vagrant
      # vault
      vbindiff
      vdpauinfo
      # veracrypt
      # victoriametrics
      vim
      # virtualbox # do not enable! virtualisation.virtualbox.host.enable = true is enough. weird erros occur.
      # virt-manager
      # virt-top
      virt-viewer
      vlc
      # vulnix
      vscode
      # vnstat
      # vscode
      wavemon # wifi diag
      wget
      which
      # wire-desktop
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
      xournalpp
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
      zbar # decode/parse qr codes
      # zfs
      zip
      # zoom-us
      # zsync
      # mobile-broadband-provider-info usb-modeswitch usb-modeswitch-data # TODO: will we need this?

      backup
      eyaml
      gcd
      gco
      git-recover
      gyro2bb
      mpvdash
      mvb2sum
      sendkeys
    ];

  environment.pathsToLink = [ "share/thumbnailers" ];

  fonts.packages = with pkgs; [
    cantarell-fonts
    gentium
    noto-fonts-color-emoji
    source-code-pro
    twitter-color-emoji
  ];

}
