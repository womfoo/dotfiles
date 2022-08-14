{ config, pkgs, ... }:

{
  imports =
    [
      # ../shared/anonwifi.nix
      ../shared/common.nix
      ../shared/desktop-apps.nix
      ../shared/gikos-kranium.nix
      ./hardware-configuration.nix
    ];

  boot.extraModulePackages = [ config.boot.kernelPackages.v4l2loopback ];
  boot.kernelModules = [ "v4l2loopback" "snd-aloop" ];
  #boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernelParams = ["intel_pstate=disable"];
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.systemd-boot.enable = true;
  boot.tmpOnTmpfs = true;

  fileSystems."/armorydata" =
    { device = "habilog.gikos.net:/armorydata";
      fsType = "nfs";
      options = ["auto" "nofail" "soft"];
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
  services.fwupd.enable = true;
  services.fwupd.enableTestRemote = true;
  services.hardware.bolt.enable = true;
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
  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_13;
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
  };
  services.rpcbind.enable = true; # needed for NFS client
  services.tlp.enable = true;
  services.touchegg.enable = true;
  services.upower.enable = true;
  services.xserver.desktopManager.xterm.enable = false;
  services.xserver.displayManager.defaultSession = "none+xmonad";
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.enable = true;
  services.xserver.libinput.enable = true;
  services.xserver.synaptics.enable = false;
  services.xserver.videoDrivers = [ "nvidia" ];
  services.xserver.windowManager.xmonad.enable = true;
  services.xserver.windowManager.xmonad.enableContribAndExtras = true;

  system.stateVersion = "22.05";

  virtualisation.docker.enable = true;
  virtualisation.libvirtd.enable = true;
  virtualisation.podman.enable = true;

}
