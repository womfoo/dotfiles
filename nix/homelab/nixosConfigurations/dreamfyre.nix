# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).
# { config, lib, pkgs, ... }:
{

  #networking.firewall.allowedTCPPorts = [ 6443 ];
  #services.k3s.enable = true;

  bee.system = "aarch64-linux";
  bee.pkgs = import inputs.jetpack-nixos.inputs.nixpkgs {
    inherit (inputs.nixpkgs) system;
    config.allowBroken = true;
    config.allowUnfree = true;
    overlays = [
      (import (inputs.jetpack-nixos + "/overlay.nix"))
      (import (inputs.jetpack-nixos + "/overlay-with-config.nix") config) # inputs.nur.overlay
      #cell.overlays.x86_64
      inputs.nur.overlays.default
    ];
  };

  imports = [
    inputs.home-24-11.nixosModule
    cell.hardwareProfiles.dreamfyre
    inputs.jetpack-nixos.nixosModules.default
    cell.nixosModules.desktop-apps
    cell.nixosModules.gikos-kranium
    cell.nixosModules.gikos-kranium-hm
    cell.nixosModules.common
  ];

  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot";
  boot.loader.systemd-boot.enable = true;

  hardware.bluetooth.enable = true;
  hardware.nvidia-jetpack.bootloader.autoUpdate = false;
  hardware.nvidia-jetpack.carrierBoard = "devkit";
  hardware.nvidia-jetpack.enable = true;
  hardware.nvidia-jetpack.modesetting.enable = false;
  hardware.nvidia-jetpack.som = "orin-agx"; # Other options include orin-agx, xavier-nx, and xavier-nx-emmc
  hardware.nvidia.open = false;
  hardware.opengl.enable = true;

  networking.hostName = "dreamfyre";

  security.sudo.wheelNeedsPassword = false;

  services.displayManager.defaultSession = "none+xmonad";
  services.nvpmodel.profileNumber = 0;
  services.ollama.acceleration = "cuda";
  services.ollama.enable = true;
  services.openssh.enable = true;
  services.pipewire.enable = true;
  services.pipewire.pulse.enable = true;
  services.xserver.desktopManager.xterm.enable = false;
  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.enable = true;
  services.xserver.monitorSection = ''
    Option "DPMS" "true"
  '';
  services.xserver.windowManager.xmonad.enable = true;
  services.xserver.windowManager.xmonad.enableContribAndExtras = true;

  system.stateVersion = "24.11";

  users.extraUsers.lightdm = {
    extraGroups = [
      "video"
    ];
  };

  virtualisation.docker.enable = true;

}
