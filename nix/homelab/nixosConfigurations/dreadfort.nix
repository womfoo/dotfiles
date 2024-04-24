{ pkgs, lib, ... }:
{
  bee.system = "x86_64-linux";
  bee.pkgs = import inputs.nixos {
    inherit (inputs.nixpkgs) system;
    allowUnfree = true;
    # nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    #   "broadcom-sta"
    # ];


  };
  imports = [

    inputs.disko.nixosModules.disko
    cell.diskoConfigurations.dreadfort

    cell.hardwareProfiles.dreadfort
    cell.nixosModules.common
    cell.nixosModules.gikos-kranium
    cell.nixosModules.myk3s
  ];
  services.acpid.enable = true;
  services.upower.enable = true;
  system.build.myInit = pkgs.runCommand "init" { } ''
    mkdir -p $out
    echo -n "init=${config.system.build.toplevel}/init initrd=initrd loglevel=4" > $out/init
  '';
  # virtualisation.libvirtd.enable = true;
  # virtualisation.virtualbox.host.enable = true;
  networking.hostName = "dreadfort";
  system.stateVersion = "24.05";

  services.getty.autologinUser = lib.mkForce "root";
  services.myk3s.enable = true;

}
