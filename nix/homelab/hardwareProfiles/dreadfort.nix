{ config, pkgs, lib, modulesPath, ... }:
{
  imports = [
    (modulesPath + "/installer/netboot/netboot-minimal.nix")
  ];
  boot.kernelModules = [ "kvm-intel" /* "wl" */ ];
  # boot.extraModulePackages = [ config.boot.kernelPackages.broadcom_sta ];
  hardware.cpu.intel.updateMicrocode = true;
  powerManagement.cpuFreqGovernor = "performance";
  hardware.enableRedistributableFirmware = true;
}
