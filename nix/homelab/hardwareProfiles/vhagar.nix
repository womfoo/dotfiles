{
  # warning: impurities
  boot.binfmt.emulatedSystems = [
    "armv7l-linux"
    "aarch64-linux"
  ];
  # boot.extraModulePackages = [ config.boot.kernelPackages.v4l2loopback config.boot.kernelPackages.rtl8814au ];
  boot.kernelModules = [
    "kvm-intel"
    "snd-aloop"
    "v4l2loopback"
  ];
  boot.extraModprobeConfig = ''
    options thinkpad_acpi fan_control=1
  '';
  boot.kernelPackages = pkgs.linuxPackages_latest;
  # boot.kernelPackages = pkgs.linuxPackages_6_8;
  boot.kernelParams = [
    "intel_pstate=disable"
    "intel_iommu=on"
    "vme_core.default_ps_max_latency_us=5500"
  ];
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.systemd-boot.enable = true;
  boot.supportedFilesystems = [ "zfs" ];
  boot.extraModulePackages = [ ];
  boot.initrd.availableKernelModules = [
    "xhci_pci"
    "nvme"
    "usbhid"
    "usb_storage"
    "sd_mod"
    "rtsx_pci_sdmmc"
  ];
  boot.initrd.kernelModules = [ ];
  boot.initrd.luks.devices."crpytme".device = "/dev/disk/by-uuid/760e36d7-90af-43da-8102-0dc291663cb7";
  fileSystems."/" = {
    device = "/dev/mapper/crpytme";
    fsType = "btrfs";
  };
  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/5ABC-99F8";
    fsType = "vfat";
  };
  fileSystems."/home/kranium/.local/share/Daedalus" = {
    device = "data/daedalus";
    fsType = "zfs";
  };
  fileSystems."/var/lib/docker" = {
    device = "data/docker";
    fsType = "zfs";
  };
  fileSystems."/var/lib/libvirt" = {
    device = "data/libvirt";
    fsType = "zfs";
  };
  fileSystems."/var/lib/postgresql" = {
    device = "data/postgresql";
    fsType = "zfs";
  };
  fileSystems."/var/lib/rancher" = {
    device = "data/rancher";
    fsType = "zfs";
  };
  fileSystems."/var/lib/private/ollama" = {
    device = "data/ollama";
    fsType = "zfs";
  };
  fileSystems."/zfstemp" = {
    device = "data/temp";
    fsType = "zfs";
  };
  hardware.bluetooth.enable = true;
  hardware.cpu.intel.updateMicrocode = true;
  hardware.graphics = {
    extraPackages = with pkgs; [
      libvdpau-va-gl
      vaapiVdpau
    ];
  };
  # hardware.nvidia.modesetting.enable = true;
  hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.stable;
  # hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.beta;
  hardware.nvidia.open = true;
  # hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.beta;
  hardware.sane.drivers.scanSnap.enable = true;
  hardware.sane.enable = true;
  hardware.enableAllFirmware = true;
  powerManagement.cpuFreqGovernor = "performance"; # defaults to powersave
  swapDevices = [ ];
}
