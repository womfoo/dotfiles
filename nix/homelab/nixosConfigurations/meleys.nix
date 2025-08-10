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
  virtualisation.azureImage.vmGeneration = "v2";
  imports = [
    (inputs.nixpkgs + /nixos/modules/virtualisation/azure-image.nix)
    cell.nixosModules.common
    cell.nixosModules.gikos-kranium
    # inputs.srvos.nixosModules.mixins-nginx
    inputs.srvos.nixosModules.mixins-telegraf
    inputs.srvos.nixosModules.server
  ];

  networking.hostName = "meleys";

  environment.systemPackages = with pkgs; [ git ];
}
