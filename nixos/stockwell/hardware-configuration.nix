# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [ (modulesPath + "/installer/scan/not-detected.nix")
    ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" "wl" ];
  boot.extraModulePackages = [ config.boot.kernelPackages.broadcom_sta ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/08c10dd1-a2eb-4efb-8a33-b633ecb77bbf";
      fsType = "ext4";
    };

  boot.initrd.luks.devices."nixos_x86-64_usb".device = "/dev/disk/by-uuid/d2e9313f-6fe9-45c2-b13f-90d9d86dba17";

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/AFB4-0BB9";
      fsType = "vfat";
    };

  swapDevices = [ ];

  # high-resolution display
  hardware.video.hidpi.enable = lib.mkDefault true;
}