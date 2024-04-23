{
  boot.initrd.availableKernelModules = [
    "xhci_pci"
    "ahci"
    "nvme"
    "usb_storage"
    "sd_mod"
  ];
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.systemd-boot.enable = true;
  fileSystems."/" = {
    device = "/dev/disk/by-uuid/cf7b96d0-487d-44f1-8f07-b2c99cbf00c7";
    fsType = "ext4";
  };
  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/27E1-CBFB";
    fsType = "vfat";
  };
  networking.interfaces.enp0s31f6.useDHCP = true;
}
