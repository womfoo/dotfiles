{ config, lib, pkgs, ... }:
let
  noplay = false;
  # noplay = true;
in
{
  imports =
    [
      # ../shared/anonwifi.nix
      ../shared/common.nix
      ../shared/desktop-apps.nix
      ../shared/gikos-kranium.nix
      ../shared/gikos-kranium-hm.nix
      ./hardware-configuration.nix
    ];

  boot.extraModulePackages = [ config.boot.kernelPackages.v4l2loopback config.boot.kernelPackages.rtl8814au ];
  boot.kernelModules = [ "v4l2loopback" "snd-aloop" ];
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernelParams = ["intel_pstate=disable"];
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.systemd-boot.enable = true;
  boot.supportedFilesystems = [ "zfs" ];
  boot.tmpOnTmpfs = true;

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
  fileSystems."/var/lib/postgresql" =
    { device = "data/postgresql";
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

  hardware.bluetooth.enable = true;
  hardware.cpu.intel.updateMicrocode = true;
  hardware.nvidia.modesetting.enable = true;
  hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.stable;
  # for steam to work
  hardware.opengl = {
    driSupport = true;
    # driSupport32Bit = true;
    extraPackages = with pkgs; [ libvdpau-va-gl vaapiVdpau ];
  };

  nixpkgs = {
    overlays = [
      (import ../shared/overlay-x86_64.nix)
    ];
    config = {
      allowBroken = true;
      allowUnfree = true;
    };
  };

  nix.settings.cores = 4;
  # nix.settings.max-jobs = lib.mkDefault 8;
  nix.distributedBuilds = true;

  networking.firewall.allowedTCPPorts = [
    config.services.nginx.defaultHTTPListenPort
    config.services.nginx.defaultSSLListenPort
    config.services.postgresql.port
  ];

  networking.hostId = "f0670973";
  networking.hostName = "vhagar";
  networking.networkmanager.enable = true;
  networking.networkmanager.wifi.macAddress = "random";
  networking.networkmanager.extraConfig = ''
    hostname-mode=none
  '';

  powerManagement.cpuFreqGovernor = "performance"; # defaults to powersave

  services.acpid.enable = true;
  services.arbtt.enable = true;
  # services.arbtt.package = pkgs.haskell.packages.ghc8104.arbtt;
  services.avahi.enable = true;
  services.avahi.nssmdns = true; # needed for printing
  #services.fprintd.enable = true;
  services.fwupd.enable = true;
  services.fwupd.enableTestRemote = true;
  services.hardware.bolt.enable = true;
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
  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_13;
    enableTCPIP = true;
    # extraPlugins = with pkgs.postgresql_13.pkgs; [ postgis pg_repack ];
    ensureDatabases = [
      "dbsync_mainnet"
      "dbsync_testnet"
      "dbsync_preprod"
    ];
    ensureUsers = [
      { name = "dbsync_mainnet"; ensurePermissions = { "DATABASE dbsync_mainnet" = "ALL PRIVILEGES"; }; }
      { name = "dbsync_testnet"; ensurePermissions = { "DATABASE dbsync_testnet" = "ALL PRIVILEGES"; }; }
      { name = "dbsync_preprod"; ensurePermissions = { "DATABASE dbsync_preprod" = "ALL PRIVILEGES"; }; }
    ];
    settings = {
      shared_preload_libraries = "pg_stat_statements";
      "pg_stat_statements.track" = "all";
    };
    authentication = ''
    # Generated file; do not edit!
    # TYPE  DATABASE        USER            ADDRESS                 METHOD
    local   all             all                                     trust
    host    all             all             127.0.0.1/32            trust
    host    all             all             ::1/128                 trust
    host    all             all             172.17.0.0/16           trust
    '';

  };
  services.printing.enable = true;
  services.printing.drivers = [ pkgs.hplipWithPlugin ];
  services.rpcbind.enable = true; # needed for NFS client
  services.tlp.enable = true;
  services.touchegg.enable = true;
  services.udisks2.enable = true; # needed for calibre
  services.upower.enable = true;
  services.vault.enable = true;
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
      # FIXME: storage-driver zfs?
      live-restore = true;
      bip = "10.9.8.7/16"; # state library doesn't like default but this is still adding the ff:
    };
  };
  virtualisation.libvirtd.enable = true;
  virtualisation.podman.enable = true;

}

