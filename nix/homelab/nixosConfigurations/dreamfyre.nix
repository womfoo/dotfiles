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
      (import (inputs.jetpack-nixos + "/overlay-with-config.nix") config)
      inputs.nur.overlays.default
    ];
  };

  boot.loader.efi.efiSysMountPoint = "/boot"; # TODO: why cant this be in hardwareProfiles

  imports = [
    cell.hardwareProfiles.dreamfyre
    cell.nixosModules.common
    cell.nixosModules.desktop-apps
    cell.nixosModules.gikos-kranium
    cell.nixosModules.gikos-kranium-hm
    inputs.home-24-11.nixosModule
    inputs.jetpack-nixos.nixosModules.default
    inputs.srvos.nixosModules.mixins-telegraf
  ];

  networking.hostName = "dreamfyre";
  networking.firewall.allowedTCPPorts = [ 9273 ];
  networking.useDHCP = lib.mkDefault true;

  nix.buildMachines = [
    {
      hostName = "nix-remote-builder@${inputs.lihim.lihim.constants.devices.vhagar.interfaces.lan.ip}";
      system = "x86_64-linux";
      protocol = "ssh-ng";
      # speedFactor = 5;
      maxJobs = 4;
      supportedFeatures = [
        "benchmark"
        "big-parallel"
        "kvm"
        "nixos-test"
      ];
      # mandatoryFeatures = [ ];
    }
  ];

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
