{ config, pkgs, lib, ... }:

{
  imports = [
    ./modules/fancontrol.nix
  #  ./modules/heartbeat.nix
  #  ./modules/ups.nix
  #  ./modules/usbnet.nix
  ];

  nixpkgs.overlays = [
    (import ./overlay.nix)
  ];

  boot.kernelParams = lib.mkAfter [
    "console=ttyS2,115200n8"
    "earlyprintk"
    "earlycon=uart8250,mmio32,0xff1a0000"
  ];

  #boot.kernelPackages = pkgs.linuxPackagesFor pkgs.linux_5_10_helios64;
  #boot.kernelPackages = pkgs.linuxPackagesFor pkgs.linux_6_2;
  #boot.kernelPackages = pkgs.linuxPackagesFor pkgs.linux_6_1;
  boot.kernelPackages = pkgs.linuxPackagesFor pkgs.linux_5_15;
  #boot.kernelPackages = pkgs.linuxPackages_latest;
}
