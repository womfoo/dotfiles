{

  nixpkgs.config.allowUnfree = true;

  imports = [
    inputs.jetpack-nixos.nixosModules.default
    inputs.home.nixosModules.home-manager
    cell.nixosModules.common
    cell.nixosModules.desktop-apps
    cell.nixosModules.gikos-kranium
  ];

  networking.hostName = "dreamfyre";
  networking.firewall.allowedTCPPorts = [ 9273 ];
  networking.useDHCP = lib.mkDefault true;

  services.displayManager.defaultSession = "none+xmonad";
  services.nvpmodel.profileNumber = 0;
  # services.ollama.acceleration = "cuda";
  # services.ollama.enable = true;
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
