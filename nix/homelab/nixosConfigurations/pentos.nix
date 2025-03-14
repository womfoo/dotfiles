{
  pkgs,
  config,
  ...
}:
{
  bee.system = "x86_64-linux";
  bee.pkgs = import inputs.nixpkgs {
    inherit (inputs.nixpkgs) system;
    allowUnfree = true;
  };
  hardware.cpu.intel.updateMicrocode = true;
  hardware.enableRedistributableFirmware = true;
  imports = [
    (inputs.nixpkgs + /nixos/modules/virtualisation/oci-image.nix)
    cell.nixosModules.common
    cell.nixosModules.gikos-kranium
    inputs.srvos.nixosModules.mixins-telegraf
    inputs.srvos.nixosModules.server
  ];
  # networking.firewall.interfaces.enp0s31f6.allowedUDPPorts = [ 51820 ];
  # networking.firewall.interfaces.enp0s20f0u3u2.allowedTCPPorts = [ 9090 ];
  networking.hostName = "pentos";
  users.mutableUsers = pkgs.lib.mkForce true;
}
