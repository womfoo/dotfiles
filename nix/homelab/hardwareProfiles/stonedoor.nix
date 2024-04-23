{
  boot.initrd.availableKernelModules = [
    "ahci"
    "xhci_pci"
    "virtio_pci"
    "sr_mod"
    "virtio_blk"
  ];
  boot.loader.grub.device = "/dev/vda";
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  fileSystems."/" = {
    device = "/dev/disk/by-uuid/c09ac9c9-20df-459a-ab34-22bbeb1801d7";
    fsType = "ext4";
  };
}
