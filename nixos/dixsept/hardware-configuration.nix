# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, ... }:

{
  imports = [ ];

  boot.initrd.availableKernelModules = [ "ahci" "usbhid" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/edc47d40-5337-4ff0-86f0-1f8c734c3cc2";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/eed4fab0-507e-402b-90e9-29d8c8ec64ba";
      fsType = "ext2";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/8d02908a-975b-4c36-841e-21c877344970"; }
    ];

  nix.maxJobs = 8;
}