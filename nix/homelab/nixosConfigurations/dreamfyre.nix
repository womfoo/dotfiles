# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).
# { config, lib, pkgs, ... }:
{
  virtualisation.docker.enable = true;

  #networking.firewall.allowedTCPPorts = [ 6443 ];
  #services.k3s.enable = true;

  bee.system = "aarch64-linux";
  # bee.pkgs = import inputs.nixpkgs {
  #   inherit (inputs.nixpkgs) system;

  bee.pkgs = import inputs.jetpack-nixos.inputs.nixpkgs {
    inherit (inputs.nixpkgs) system;
    config.allowBroken = true;
    config.allowUnfree = true;
    overlays = [
      (import (inputs.jetpack-nixos + "/overlay.nix"))
      (import (inputs.jetpack-nixos + "/overlay-with-config.nix") config)
      #inputs.nur.overlay
      #cell.overlays.x86_64
      inputs.nur.overlays.default
    ];
  };

  services.ollama.enable = true;
  services.ollama.acceleration = "cuda";

  environment.systemPackages = with pkgs; [
    vim
    #cant#nvidia-jetpack.uefiCapsuleUpdate
  ];

  # hardware.nvidia.open = false;

  #hardware.nvidia-jetpack.bootloader.autoUpdate = true;

  imports = [
    # Include the results of the hardware scan.
    # (builtins.fetchTarball "https://github.com/anduril/jetpack-nixos/archive/master.tar.gz" + "/modules/default.nix")
    inputs.home.nixosModule
    inputs.jetpack-nixos.nixosModules.default
    cell.hardwareProfiles.dreamfyre
    cell.nixosModules.desktop-apps
    cell.nixosModules.gikos-kranium
    cell.nixosModules.gikos-kranium-hm
    cell.nixosModules.common
  ];

  services.displayManager.defaultSession = "none+xmonad";
  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.desktopManager.xterm.enable = false;
  services.xserver.enable = true;

  services.xserver.videoDrivers = [ "nvidia" ];
  # services.xserver.wacom.enable = true; # havent figured out the eraser yet

  services.xserver.windowManager.xmonad.enable = true;
  services.xserver.windowManager.xmonad.enableContribAndExtras = true;

  # hardware.nvidia-jetpack.modesetting.enable = false; # comment this
  /*
      services.xserver = {
        enable = true;
        layout = "us";
    #    videoDrivers = [ "nvidia" ];
        displayManager.lightdm.enable = true;
        windowManager.i3.enable = true;
      };
  */

  #  system.copySystemConfiguration = true;
  /*
    users.extraUsers.kranium = {
      name = "kranium";
      extraGroups = [
        "wheel"
      ];
      group = "users";
      uid = 2000;
      createHome = true;
      home = "/home/kranium";
      shell = "/run/current-system/sw/bin/bash";
      isNormalUser = true;
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCv8Rdf8gqewljlONxIU/NoI+aQhA0UNQbAsif0gKqGLPG2QrZgPktgG3r0Fn6cKtuhy7iExfWmUafJU73Od/hj8DK6uxicHEXh5pv6DZc2DEwyC5orJHQOZLblo96u2xsBkVx/++Nq/2vW1aMN0Wg8/Vgal1fBcfJAT9XAFmiKXLZxIvxWWw0PZYil4QJtlVGwebXm1trPr7H9hV8l+Lse8Z/Xt38DzQJI7yV5m6ENxPL/xCFsMMgb27c+Xf6gJPq2DIcUOJiP7fOcHXWN2W4/+ApUH5adMhJ8Y8mT4CGcLqNhcHKSFzPaUQpfQ0vi3QJez1LYoHANu6Iy6q7HoIab kranium@silverspark"
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDYKG2PDhn+QeHVu596kNH57IziNANyhhXZIMcviWFzyd5lVLmyn41e3CUQkQUEguU+mgfrn1v3L/YAPTle/oA4bhwwoRj0nghZ13pkapNoXroAQUYlyXs4ngva1o5r3dZ7sqK+PboOFg1/HfrHKmVXqRRIOIKNUq0gLZLoe8Ga4UY/A5h+5NpHAo+uip4nPoKmHIX5e1ugAxXiKQ3sP+Ud3/wFftswyTDL1hllBeYuvbNzQIHvbtUr0e/gx54n1ETzcWu5BKrrBWyU4H4dwM9Cm0KJvhstdAcndj+BWeBZ+gmY4e6ESfZpLk9qO6LodZhXhFsavjET1miBCsFrUEHcSFBXpSzZ5djCFvN/QckYpXW46wouwbyZyBh2wgPc7o5jBdEMkCYEeec/TFfvuYAhLKASJM3vVK5vjAV1MAhnA9xKvR+jFLsi/6F88S4PxhYbIkBJ+YF3YAzdUVrduFUP8hFAZ6JTlzCAg8R+dNtCJHoaIkKsbaIgvUeiWBCTJG0= kranium@tessarion.local"
      ];
    };
  */
  security.sudo.wheelNeedsPassword = false;

  hardware.nvidia-jetpack.enable = true;
  hardware.nvidia-jetpack.som = "orin-agx"; # Other options include orin-agx, xavier-nx, and xavier-nx-emmc
  hardware.nvidia-jetpack.carrierBoard = "devkit";

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "dreamfyre"; # Define your hostname.
  # Pick only one of the below networking options.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  # networking.networkmanager.enable = true;  # Easiest to use and most distros use this by default.

  # Set your time zone.
  # time.timeZone = "Europe/Amsterdam";

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  #   useXkbConfig = true; # use xkb.options in tty.
  # };

  # Enable the X11 windowing system.
  # services.xserver.enable = true;

  # Configure keymap in X11
  # services.xserver.xkb.layout = "us";
  # services.xserver.xkb.options = "eurosign:e,caps:escape";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  # hardware.pulseaudio.enable = true;
  # OR
  # services.pipewire = {
  #   enable = true;
  #   pulse.enable = true;
  # };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  # users.users.alice = {
  #   isNormalUser = true;
  #   extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  #   packages = with pkgs; [
  #     firefox
  #     tree
  #   ];
  # };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  # environment.systemPackages = with pkgs; [
  #   vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
  #   wget
  # ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = false;

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  # system.copySystemConfiguration = true;

  # This option defines the first version of NixOS you have installed on this particular machine,
  # and is used to maintain compatibility with application data (e.g. databases) created on older NixOS versions.
  #
  # Most users should NEVER change this value after the initial install, for any reason,
  # even if you've upgraded your system to a new NixOS release.
  #
  # This value does NOT affect the Nixpkgs version your packages and OS are pulled from,
  # so changing it will NOT upgrade your system - see https://nixos.org/manual/nixos/stable/#sec-upgrading for how
  # to actually do that.
  #
  # This value being lower than the current NixOS release does NOT mean your system is
  # out of date, out of support, or vulnerable.
  #
  # Do NOT change this value unless you have manually inspected all the changes it would make to your configuration,
  # and migrated your data accordingly.
  #
  # For more information, see `man configuration.nix` or https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
  system.stateVersion = "24.11"; # Did you read the comment?

  #hardware.pulseaudio.enable = true;

  services.pipewire.enable = true;
  services.pipewire.pulse.enable = true;

  hardware.bluetooth.enable = true;
}
