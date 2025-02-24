let
  noplay = false;
  faketrunkcombined = pkgs.stdenv.mkDerivation {
    name = "localhost-spfiles";
    src = ../../homelab/fake;
    installPhase = ''
      mkdir -p $out/fakehydra
      cp trunk-combined $out/fakehydra
    '';
  };
in
{
  bee.system = "x86_64-linux";
  bee.pkgs = import inputs.nixpkgs {
    inherit (inputs.nixpkgs) system;
    config.allowBroken = true;
    config.allowUnfree = true;
    overlays = [
      inputs.nur.overlays.default
      cell.overlays.x86_64
      cell.overlays.rtmpOverlay
    ];
  };
  environment.variables = {
    MOZ_USE_XINPUT2 = "1";
  };
  imports = [
    inputs.home.nixosModule
    cell.nixosModules.common
    cell.nixosModules.builder
    cell.nixosModules.desktop-apps
    cell.nixosModules.desktop-apps-x86_64
    cell.nixosModules.gikos-kranium
    cell.nixosModules.gikos-kranium-hm
    cell.nixosModules.gikos-dockertest
    cell.nixosModules.rtmp
    cell.hardwareProfiles.vhagar
    inputs.srvos.nixosModules.mixins-telegraf
  ];

  nix.settings.cores = 10;
  nix.settings.max-jobs = lib.mkDefault 4;
  nix.distributedBuilds = true;
  networking.extraHosts =
    ''
      127.0.0.1 tahanan
    ''
    + lib.optionalString noplay ''
      127.0.0.1 laarc.io
      127.0.0.1 lobste.rs
      127.0.0.1 news.ycombinator.com
      127.0.0.1 slashdot.org
      127.0.0.1 www.youtube.com
      127.0.0.1 twitter.com
      127.0.0.1 www.reddit.com
    '';
  networking.useNetworkd = true;
  networking.firewall = {
    allowedTCPPorts = [
      1935
      1936
      8581
      48302
      51203
    ];
  };
  networking.firewall.trustedInterfaces = [ "cni+" ]; # k3s
  networking.firewall.interfaces.enp9s0u2u1u2 = {
    allowedTCPPorts = [
      80
      443
      9273
      9977
    ];
  };
  networking.firewall.interfaces."virbr1" = {
    allowedTCPPorts = [
      2049
      config.services.nfs.server.statdPort
      config.services.nfs.server.mountdPort
      config.services.nfs.server.lockdPort
    ];
    allowedUDPPorts = [
      2049
      config.services.nfs.server.statdPort
      config.services.nfs.server.mountdPort
      config.services.nfs.server.lockdPort
    ];
  };
  networking.hostId = "f0670973";
  networking.hostName = "vhagar";
  networking.networkmanager.enable = true;
  networking.networkmanager.logLevel = "TRACE";
  networking.networkmanager.wifi.macAddress = "random";
  security.polkit.enable = true;
  services.acpid.enable = true;
  services.arbtt.enable = true;
  services.atd.enable = true; # at for alarms
  services.avahi.enable = true;
  services.avahi.nssmdns4 = true; # needed for printing
  services.dbus.packages = [ pkgs.gcr ];
  #services.fprintd.enable = true;
  services.fwupd.enable = true;
  # services.fwupd.enableTestRemote = true;
  # services.grafana.enable = true;
  services.hardware.bolt.enable = true;
  # services.k3s.enable = true;
  # services.k3s.extraFlags = toString [
  #  "--disable traefik"
  #  "--kubelet-arg=eviction-hard=imagefs.available<2%,nodefs.available<2%"
  #  "--kubelet-arg=eviction-minimum-reclaim=imagefs.available=2%,nodefs.available=2%"
  # ];
  # services.libinput.enable = true;
  services.locate.enable = true;
  services.nfs.server.enable = true;
  services.nfs.server.statdPort = 47000;
  services.nfs.server.mountdPort = 47001;
  services.nfs.server.lockdPort = 47002;
  services.nfs.server.exports = ''
    # virbr1 iface
    /home/kranium/vagrantboxen/debian-bookworm64 192.168.121.1/24(rw,no_root_squash,no_subtree_check)
  '';
  services.nginx = {
    virtualHosts.localhost = {
      locations = {
        "/" = {
          root = faketrunkcombined;
        };
      };
    };
  };
  services.ollama.enable = true;
  services.ollama.acceleration = "cuda";
  services.ollama.loadModels = [
    "gemma2:2b"
    "llama3.1"
    "deepseek-r1:1.5b"
    "deepseek-r1:7b"
  ];
  services.open-webui = {
    enable = true;
    environment = {
      WEBUI_AUTH = "False";
    };
  };
  services.paperless.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
    # config.pipewire = {
    #   "context.properties" = {
    #     "default.clock.rate" = 192000;
    #     "default.clock.allowed-rates" = [ 44100 48000 96000 192000 ];
    #   };
    # };
  };
  services.printing.enable = true;
  services.printing.drivers = [ pkgs.hplipWithPlugin ];
  services.rpcbind.enable = true; # needed for NFS client
  services.saned.enable = true;
  services.smartd.enable = true;
  services.telegraf.extraConfig.inputs.upsd = { };
  services.tlp.enable = true;
  services.touchegg.enable = true;
  services.thinkfan.enable = true;
  # https://github.com/keyboardio/Chrysalis/blob/master/static/udev/60-kaleidoscope.rules
  services.udev.extraRules = ''
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="1209", ATTRS{idProduct}=="2300", SYMLINK+="model01", ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_CANDIDATE}="0", TAG+="uaccess", TAG+="seat"
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="1209", ATTRS{idProduct}=="2301", SYMLINK+="model01", ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_CANDIDATE}="0", TAG+="uaccess", TAG+="seat"
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="1209", ATTRS{idProduct}=="2302", SYMLINK+="Atreus2", ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_CANDIDATE}="0", TAG+="uaccess", TAG+="seat"
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="1209", ATTRS{idProduct}=="2303", SYMLINK+="Atreus2", ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_CANDIDATE}="0", TAG+="uaccess", TAG+="seat"
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="3496", ATTRS{idProduct}=="0005", SYMLINK+="model100", ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_CANDIDATE}="0", TAG+="uaccess", TAG+="seat"
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="3496", ATTRS{idProduct}=="0006", SYMLINK+="model100", ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_CANDIDATE}="0", TAG+="uaccess", TAG+="seat"
  '';
  services.udisks2.enable = true; # needed for calibre
  services.usbmuxd.enable = true; # for ifuse/ios tethering
  services.upower.enable = true;
  services.displayManager.defaultSession = "none+xmonad";
  services.displayManager.sddm.enable = true;
  services.xserver.desktopManager.xterm.enable = false;
  services.xserver.enable = true;
  services.xserver.synaptics.enable = false;
  services.xserver.videoDrivers = [ "nvidia" ];
  services.xserver.wacom.enable = true; # havent figured out the eraser yet
  services.xserver.windowManager.xmonad.enable = true;
  services.xserver.windowManager.xmonad.enableContribAndExtras = true;
  system.stateVersion = "24.11";
  systemd.watchdog.device = "/dev/watchdog";
  virtualisation.docker = {
    enable = true;
    daemon.settings = {
      group = "docker";
      hosts = [ "fd://" ];
      log-driver = "journald";
      # storage-driver zfs?
      live-restore = true;
      # runtimes play with nvidia runtime?
      # bip = "10.9.8.7/16"; # state library doesn't like default but this is still adding the ff:
    };
  };
  virtualisation.libvirtd.enable = true;
  virtualisation.podman.enable = true;
  /*
    power.ups = {
      enable = true;
      ups.slimlineups = {
        port = "/dev/ttyUSB0";
        # port = "/dev/ttyUSB1";
        driver = "nutdrv_qx"; # "blazer_usb" does not work;
      };
      upsmon = {
        enable = true;
        # upsmon.settings.MINSUPPLIES = 1;
        monitor.slimlineups = {
          user = "admin";
          passwordFile = "wtf";
        };
      };
    };
  */

  # stolen from srvos

  # The notion of "online" is a broken concept
  # https://github.com/systemd/systemd/blob/e1b45a756f71deac8c1aa9a008bd0dab47f64777/NEWS#L13
  systemd.services.NetworkManager-wait-online.enable = false;
  systemd.network.wait-online.enable = false;

  # FIXME: Maybe upstream?
  # Do not take down the network for too long when upgrading,
  # This also prevents failures of services that are restarted instead of stopped.
  # It will use `systemctl restart` rather than stopping it with `systemctl stop`
  # followed by a delayed `systemctl start`.
  systemd.services.systemd-networkd.stopIfChanged = false;
  # Services that are only restarted might be not able to resolve when resolved is stopped before
  systemd.services.systemd-resolved.stopIfChanged = false;

}
