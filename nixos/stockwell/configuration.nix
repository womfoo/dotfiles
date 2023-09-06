# FIXME: this is still pretty rough, based on silverspark
{ config, options, lib, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ../shared/common.nix
      ../shared/gikos-kranium.nix
    ];

  boot.kernelPackages = pkgs.linuxPackages_latest;

  # Use the gummiboot efi boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.timeout = 4;
  boot.loader.efi.canTouchEfiVariables = true;

  networking = {
    nameservers = [ "1.1.1.1" /* "8.8.8.8" "8.8.4.4" */ ];
    hostName = "stockwell";
    networkmanager = {
      enable = true;
      dns = "none";
    };
    firewall.logRefusedPackets = true;
    firewall.allowedTCPPorts = [
      config.services.hydra.port # FIXME: move behind apache
    ];
  };

  services.acpid.enable = true;
  services.upower.enable = true;

  hardware.cpu.intel.updateMicrocode = true;

  #this tends to overheat
  powerManagement.cpuFreqGovernor = "performance";

  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.package = pkgs.pulseaudioFull; # we need to use the full package for bluetooth support

  hardware.bluetooth.enable = true;

  hardware.opengl.extraPackages = with pkgs; [ libvdpau-va-gl vaapiVdpau ];

  #services.flatpak.enable = true;
  virtualisation.virtualbox.host.enable = true;
  virtualisation.libvirtd.enable = true;

  nixpkgs.config = {
    allowBroken = true;
    allowUnfree = true;
  };

  #iphone mounting needs
  services.usbmuxd.enable = true;

  system.stateVersion = "23.11";
  nix.package = pkgs.nixUnstable;

  nix.extraOptions = ''
    keep-outputs = true
    extra-platforms = aarch64-linux
    experimental-features = nix-command flakes
  '';

}
