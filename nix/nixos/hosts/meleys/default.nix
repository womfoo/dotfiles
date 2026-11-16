{
  pkgs,
  config,
  modulesPath,
  inputs,
  self,
  ...
}:
{
  nixpkgs.config.allowUnfree = true;
  hardware.cpu.intel.updateMicrocode = true;
  hardware.enableRedistributableFirmware = true;
  virtualisation.azureImage.vmGeneration = "v2";
  imports = [
    (modulesPath + "/virtualisation/azure-image.nix")
    self.nixosModules.common
    self.nixosModules.gikos-kranium
    # inputs.srvos.nixosModules.mixins-nginx
    inputs.srvos.nixosModules.mixins-telegraf
    inputs.srvos.nixosModules.server
  ];

  networking.hostName = "meleys";

  environment.systemPackages = with pkgs; [ git ];
}
