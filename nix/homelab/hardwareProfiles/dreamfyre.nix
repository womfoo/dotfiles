{

  # boot.binfmt.emulatedSystems = [
  #   "x86_64-linux"
  # ];
  boot.extraModulePackages = [ ];
  boot.initrd.availableKernelModules = [ "nvme" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ ];
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.systemd-boot.enable = true;

  hardware.bluetooth.enable = true;
  hardware.graphics.enable = true;
  hardware.nvidia-jetpack.carrierBoard = "devkit";
  hardware.nvidia-jetpack.enable = true;
  hardware.nvidia-jetpack.firmware.autoUpdate = false;
  hardware.nvidia-jetpack.modesetting.enable = false;
  hardware.nvidia-jetpack.som = "orin-agx";
  hardware.nvidia.open = false;

  fileSystems."/boot" = {
    device = "/dev/nvme0n1p1";
    fsType = "vfat";
    options = [
      "fmask=0022"
      "dmask=0022"
    ];
  };

  fileSystems."/" = {
    device = "/dev/nvme0n1p2";
    fsType = "ext4";
  };

  swapDevices = [ ];

}
