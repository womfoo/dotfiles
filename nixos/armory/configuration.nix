{ config, lib, pkgs, ... }:
let 
  #mytelegraf = pkgs.callPackage /home/kranium/go/src/github.com/svenklemm/telegraf {};
  serverIP = "0.0.0.0";
  #internalWifiInt = "wlp1s0u1u1u1"; # first slot in usbhub
  internalWifiInt = "wlp1s0u1u2u4u1"; # some slot
  #internalWifiInt = "wlp1s0u1u2"; # directly attached
  in
{
  services.hostapd = {
   enable = true;
   ssid = "resolute";
   interface = internalWifiInt;
   wpaPassphrase = "DangerWillRobinson";
  };
  services.dnsmasq = {
    enable = true;
    servers = [ "8.8.8.8" "8.8.4.4" ];
    extraConfig = ''
        domain=resolute
        interface=${internalWifiInt}
        dhcp-option=6,8.8.8.8,8.8.4.4
        dhcp-range=10.20.61.100,10.20.61.200,24h
        dhcp-host=60:f8:1d:cc:f5:e2,10.20.61.150
        dhcp-host=e0:33:8e:51:f9:5a,10.20.61.151
      
    '';
        #dhcp-host=86:74:e7:91:4d:92,10.20.61.200
        #dhcp-range=123.243.9.230,123.243.9.230,24h
        #dhcp-range=10.20.61.2,10.20.61.200,24h
  };
  networking = {
    hostName = "armory";
    supplicant.wlan0 = {
      configFile.path = "/home/supp.conf";
    };

    interfaces = {
      #eth0 = { ipv4 = { addresses = [ { address = "10.20.61.1"; prefixLength = 24; } ]; }; }; 
      #eth0 = { ipv4 = { addresses = [ { address = "123.243.9.229"; prefixLength = 30; } ]; }; }; 
      #wlp1s0u1u1u1 = { ipv4 = { addresses = [ { address = "10.20.61.1"; prefixLength = 24; } ]; }; }; 
      "${internalWifiInt}" = { ipv4 = { addresses = [ { address = "10.20.61.1"; prefixLength = 24; } ]; }; }; 
      #wlp1s0u1u2 = { ipv4 = { addresses = [ { address = "10.20.61.1"; prefixLength = 24; } ]; }; }; 
      #wlan0 = { ipv4 = { addresses = [ { address = "10.20.61.1"; prefixLength = 24; } ]; }; }; 
    };

    nat = {
      enable = true;
      externalInterface = "wlan0";
      internalInterfaces = [ internalWifiInt ];
    };


    firewall = {
      allowPing = true;
      allowedTCPPorts = [
                          80
                          24800
                        ];
      allowedUDPPorts = [ 67 ];
    };
  };
  
  # placeholder to see 
  #services.nginx.enable = true;

  services.ntp.enable = true;

  # no gui by def
  systemd.services.display-manager.wantedBy = lib.mkOverride 50 [];

  services.influxdb.enable = true;
 
  services.arbtt.enable = true;

  hardware.enableAllFirmware = true;

  environment.systemPackages = with pkgs; [
    exfat
    exfat-utils
    fuse_exfat

    barrier # keyboard thingy
    #libreoffice
    keepassx
    azure-cli
    #gdisk
    ipcalc
    oathToolkit
    yq
    xdotool
    speedtest-cli
    screen
    dmenu
    raspberrypi-tools
    #haskellPackages.darcs
    #ledger
    gitFull
    emacs
    darcs
    firefox
    hiera-eyaml
    tmux
    tmux-cssh
    trayer
    (haskellPackages.ghcWithPackages (self : with haskellPackages; with pkgs.haskell.lib; [
      arbtt
      cabal-install
      cabal2nix
      xmobar
      hledger
    ]))
    keepassx
    openconnect_openssl
    #softether
    nethogs
    iotop
    jq
    #(facter { libwhereami = null; }) # libwhereami is borken
    gparted
    #vlc
    pulsemixer
    dillo
    #(dillo.override { openssl = openssl_1_0_2; })
    #arora
    links
    vim
    mc
    st
    llvm_6 # for haskell to compile
    tcpdump
    ncdu
    nixfmt
    lsof
    hiera-eyaml
    mpv
    #vlc
    redshift
    parcellite
    #pasystray
    arandr
    iw
    glxinfo # glxgears
    chromium # no chromium build
    dep2nix
    ntfs3g
  ];
  
  services.openssh.enable = lib.mkForce true;

/*

  sdImage.compressImage  = false;
  sdImage.populateRootCommands = ''
    mkdir -p ./files/nixpkgs
    echo "trololololo" > ./files/nixpkgs/hello.txt
  '';
*/

  programs.ssh.startAgent = true;

  hardware.bluetooth.enable = true;
  
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.package = pkgs.pulseaudioFull; # we need to use the full package for bluetooth support
/*
  services.softether.enable = true;
  services.softether.vpnclient.enable = true;
*/
  #services.xserver.displayManager.lightdm.enable = true;
  #services.xserver.enable = true;
  #services.xserver.videoDrivers = [ "modesetting" ];
  boot.loader.grub.enable = false;

	boot.loader.raspberryPi.enable = true;
	boot.loader.raspberryPi.version = 4;
	#boot.loader.raspberryPi.uboot.enable = true;
	boot.kernelPackages = pkgs.linuxPackages_rpi4;

  hardware.opengl = {
    enable = true;
    setLdLibraryPath = true;
    package = pkgs.mesa_drivers;
  };
  hardware.deviceTree = {
    base = pkgs.device-tree_rpi;
    overlays = [ "${pkgs.device-tree_rpi.overlays}/vc4-fkms-v3d.dtbo" ];
  };

  boot.loader.raspberryPi.firmwareConfig = ''
    dtparam=audio=on
    dtparam=spi=on
    audio_pwm_mode=2
    #gpu_mem=192
    # Force the monitor to HDMI mode so that sound will be sent over HDMI cable
    #hdmi_drive=2
    # Set monitor mode to DMT
    hdmi_group=2
    # Make display smaller to stop text spilling off the screen
    overscan_left=0
    overscan_right=0
    overscan_top=0
    overscan_bottom=0
  '';

  systemd.services.btattach = {
    before = [ "bluetooth.service" ];
    after = [ "dev-ttyAMA0.device" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      ExecStart = "${pkgs.bluez}/bin/btattach -B /dev/ttyAMA0 -P bcm -S 3000000";
    };
  };

  #boot.consoleLogLevel = lib.mkDefault 7;

/*
  # File systems configuration for using the installer's partition layout
  fileSystems = {
    "/boot" = {
      device = "/dev/disk/by-label/NIXOS_BOOT";
      fsType = "vfat";
    };
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
    };
  };
  swapDevices =
    [ { device = "/dev/disk/by-uuid/942e5dc3-59e9-48d8-bf22-50d1ac17e179"; }
    ];

  imports = [ <nixpkgs/nixos/modules/installer/cd-dvd/sd-image-raspberrypi4.nix> ];
*/

  imports = [ ./hardware-configuration.nix];

  services.xserver = {
    enable = true;
    #videoDrivers = [ "fbdev" ]; # when gpu disabled
    videoDrivers = [ "modesetting" ];
    displayManager.lightdm.enable = true;
    #displayManager.sddm.enable = true;
    #displayManager.sddm.autoLogin.enable = true;
    #displayManager.sddm.autoLogin.user = "kranium";

    /*
    desktopManager.xfce.enable = true;
    desktopManager.xfce.extraSessionCommands = ''
      ${pkgs.firefox}/bin/firefox https://gikos.net/ &
      ${pkgs.x11vnc}/bin/x11vnc -bg -forever -noxrecord -rfbport 5900 -shared &
    '';
    */

    # windowManager.default = "xfce";

    displayManager.defaultSession = "none+xmonad";
    #desktopManager.xterm.enable = false;
    #windowManager.default = "xmonad";
    windowManager.xmonad = {
       enable = true;
       #config = /home/kranium/dotfiles/.xmonad/xmonad.hs;
       enableContribAndExtras = true;
    };

/*
*/
    # for newer unstable
    #displayManager.defaultSession = "none+i3";
    #windowManager.default = "i3";
    #windowManager.i3.enable = true;
  };

  # ‘softether-4.29’ is not supported on ‘aarch64-linux
  nixpkgs.config = {
    allowUnsupportedSystem = true;
    allowUnfree = true;

    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = hsSelf: hsSuper: {
        hint  = pkgs.haskell.lib.overrideCabal hsSuper.hint (oa: {
          doCheck = false;
          });
        };
      };
    };

  };

  time.timeZone = "Australia/Sydney";

  users.extraUsers.kranium = {
     name = "kranium";
     extraGroups = [ "wheel" "networkmanager" "audio" "docker" "vboxusers" "video" "lp" "dialout" ];
     group = "users";
     uid = 2000;
     createHome = true;
     home = "/home/kranium";
     shell = "/run/current-system/sw/bin/bash";
  };
  users.extraGroups = { kranium = { gid = 2000; } ; } ;

  #documentation.nixos.enable = false;

  security.sudo.wheelNeedsPassword = false;

  virtualisation.docker.enable = true;

  # docker-containers.pihole = {
  #   image = "pihole/pihole:latest";
  #   ports = [
  #     "${serverIP}:53:53/tcp"
  #     "${serverIP}:53:53/udp"
  #     "3080:80"
  #     "30443:443"
  #   ];
  #   volumes = [
  #     "/var/lib/pihole/:/etc/pihole/"
  #     "/var/lib/dnsmasq/.d:/etc/dnsmasq.d/"
  #   ];
  #   environment = {
  #     ServerIP = serverIP;
  #   };
  #   extraDockerOptions = [
  #     "--cap-add=NET_ADMIN"
  #     "--dns=127.0.0.1"
  #     "--dns=1.1.1.1"
  #   ];
  #   workdir = "/var/lib/pihole/";
  # };

  services.grafana = {
   enable = true;
   auth.anonymous.enable = true;
  };
  services.postgresql = {
   enable = true;
   extraPlugins = [ pkgs.timescaledb ];
   extraConfig = "shared_preload_libraries = 'timescaledb'";
 };

  # so ethernet gets enabled
  services.usbmuxd.enable = true;

  systemd.services.telegraf.path = [ pkgs.procps ]; # telegraf procstat will fail without this
  services.telegraf.enable = true;
  #services.telegraf.package = mytelegraf;
  services.telegraf.extraConfig = {
    inputs = {
      conntrack = {};
      cpu = {
        percpu = true;
        totalcpu = true;
        collect_cpu_time = false;
      };
      disk = { };
      diskio = { };
      interrupts = {};
      kernel = { };
      mem = { };
      net = { };
      netstat = { };
      system = { };
      temp = { };
      swap = { };
      processes = { };
    };
    outputs = { influxdb = { database = "telegraf"; urls = [ "http://localhost:8086" ]; }; };
    #outputs = { postgresql = { connection = "connection://metrics_writer:metricwriterlol@localhost[metrics]"; };
    #         };
  };
  
   nix.distributedBuilds = true;
   nix.buildMachines = [
    {
       hostName = "builder";
       system = "aarch64-linux";
       maxJobs = 4;

}

];



}


