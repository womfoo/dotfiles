{ config, lib, pkgs, ... }:
let
  noplay = false;
  # noplay = true;
in
{

  # boot.extraModulePackages = [ config.boot.kernelPackages.v4l2loopback config.boot.kernelPackages.rtl8814au ];
  boot.kernelModules = [ "v4l2loopback" "snd-aloop" ];
  boot.extraModprobeConfig = ''
    options thinkpad_acpi fan_control=1
  '';
  # boot.kernelPackages = pkgs.linuxPackages_latest; # zfs b0rk 6.5
  # boot.kernelPackages = pkgs.linuxPackages_6_4;
  boot.kernelParams = ["intel_pstate=disable"
                       "intel_iommu=on"
                       "vme_core.default_ps_max_latency_us=5500"
                      ];
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.systemd-boot.enable = true;
  boot.supportedFilesystems = [ "zfs" ];
  # boot.tmp.useTmpfs = true; # gone in 23.11?

  environment.variables = {
    MOZ_USE_XINPUT2 = "1";
  };

  fileSystems."/armorydata" =
    { device = "habilog.gikos.net:/armorydata";
      fsType = "nfs";
      options = ["auto" "nofail" "soft"];
    };
  fileSystems."/var/lib/docker" =
    { device = "data/docker";
      fsType = "zfs";
    };
  fileSystems."/home/kranium/.local/share/Daedalus" =
    { device = "data/daedalus";
      fsType = "zfs";
    };
  fileSystems."/zfstemp" =
    { device = "data/temp";
      fsType = "zfs";
    };
  fileSystems."/var/lib/rancher" =
    { device = "data/rancher";
      fsType = "zfs";
    };
  hardware.bluetooth.enable = true;
  hardware.cpu.intel.updateMicrocode = true;
  hardware.nvidia.modesetting.enable = true;
  hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.stable;
  hardware.sane.drivers.scanSnap.enable = true;
  hardware.sane.enable = true;
  services.saned.enable = true;
  # for steam to work
  hardware.opengl = {
    driSupport = true;
    # driSupport32Bit = true;
    extraPackages = with pkgs; [ libvdpau-va-gl vaapiVdpau ];
  };

  hardware.enableAllFirmware = true;

  nix.settings.cores = 10;
  nix.settings.max-jobs = lib.mkDefault 4;
  nix.distributedBuilds = true;

  networking.extraHosts = ''
    127.0.0.1 tahanan
  '' + lib.optionalString noplay ''
    127.0.0.1 laarc.io
    127.0.0.1 lobste.rs
    127.0.0.1 news.ycombinator.com
    127.0.0.1 slashdot.org
    127.0.0.1 www.youtube.com
  '';

  # 127.0.0.1 twitter.com # because grok
  networking.firewall.allowedTCPPorts = [
    8554
    # config.services.nginx.defaultHTTPListenPort
    # config.services.nginx.defaultSSLListenPort
  ];
  networking.firewall.trustedInterfaces = [ "cni+" ]; # k3s
  networking.hostId = "f0670973";
  networking.hostName = "vhagar";
  networking.networkmanager.enable = true;
  # networking.networkmanager.enableFccUnlock = true;
  networking.networkmanager.logLevel = "TRACE";
  networking.networkmanager.wifi.macAddress = "random";
  networking.networkmanager.extraConfig = ''
    hostname-mode=none
  '';

  powerManagement.cpuFreqGovernor = "performance"; # defaults to powersave

  services.acpid.enable = true;
  services.arbtt.enable = true;
  services.atd.enable = true; # at for alarms
  services.avahi.enable = true;
  services.avahi.nssmdns4 = true; # needed for printing
  #services.fprintd.enable = true;
  services.fwupd.enable = true;
  # services.fwupd.enableTestRemote = true;
  services.hardware.bolt.enable = true;
  services.k3s.enable = true;
  services.k3s.extraFlags = ''
    --disable traefik
    --server-arg '--kubelet-arg=eviction-hard=imagefs.available<5%,nodefs.available<5%' \
    --server-arg '--kubelet-arg=eviction-minimum-reclaim=imagefs.available=5%,nodefs.available=5%'
  '';
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
  security.polkit.enable = true;

  services.printing.enable = true;
  services.printing.drivers = [ pkgs.hplipWithPlugin ];
  services.rpcbind.enable = true; # needed for NFS client
  services.tlp.enable = true;
  services.touchegg.enable = true;
  services.thinkfan.enable = true;
  # services.nbfc.enable = true;
  # services.udev.packages = with pkgs; [ ];
  services.udisks2.enable = true; # needed for calibre
  services.usbmuxd.enable = true; # for ifuse/ios tethering
  services.upower.enable = true;
  # services.vault.enable = true;
  systemd.watchdog.device = "/dev/watchdog";
  services.xserver.desktopManager.xterm.enable = false;
  services.xserver.displayManager.defaultSession = "none+xmonad";
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.enable = true;
  services.xserver.libinput.enable = true;
  services.xserver.synaptics.enable = false;
  services.xserver.videoDrivers = [ "nvidia" ];
  services.xserver.wacom.enable = true; # havent figured out the eraser yet
  services.xserver.windowManager.xmonad.enable = true;
  services.xserver.windowManager.xmonad.enableContribAndExtras = true;

  system.stateVersion = "22.05";

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

  # warning: impurities
  boot.binfmt.emulatedSystems = [ "armv7l-linux" "aarch64-linux" ];

  services.dbus.packages = [ pkgs.gcr ];

  networking.firewall.interfaces."virbr1" = {
    allowedTCPPorts = [ 2049 ];
    allowedUDPPorts = [ 2049 ];
  };

  services.nfs.server.enable = true;
  services.nfs.server.statdPort = 47000;
  services.nfs.server.mountdPort = 47001;
  services.nfs.server.lockdPort = 47002;
  services.nfs.server.exports    = ''
    # virbr1 iface
    /home/kranium/vagrantboxen/debian-bookworm64 192.168.121.1/24(rw,no_root_squash,no_subtree_check)
  '';

  # https://github.com/keyboardio/Chrysalis/blob/master/static/udev/60-kaleidoscope.rules
  services.udev.extraRules = ''
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="1209", ATTRS{idProduct}=="2300", SYMLINK+="model01", ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_CANDIDATE}="0", TAG+="uaccess", TAG+="seat"
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="1209", ATTRS{idProduct}=="2301", SYMLINK+="model01", ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_CANDIDATE}="0", TAG+="uaccess", TAG+="seat"
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="1209", ATTRS{idProduct}=="2302", SYMLINK+="Atreus2", ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_CANDIDATE}="0", TAG+="uaccess", TAG+="seat"
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="1209", ATTRS{idProduct}=="2303", SYMLINK+="Atreus2", ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_CANDIDATE}="0", TAG+="uaccess", TAG+="seat"
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="3496", ATTRS{idProduct}=="0005", SYMLINK+="model100", ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_CANDIDATE}="0", TAG+="uaccess", TAG+="seat"
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="3496", ATTRS{idProduct}=="0006", SYMLINK+="model100", ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_CANDIDATE}="0", TAG+="uaccess", TAG+="seat"
  '';

}
