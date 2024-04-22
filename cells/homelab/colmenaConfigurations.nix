{
  vhagar = {
    networking.hostName = "vhagar";
    deployment = { allowLocalDeployment = true; };
    imports = [ cell.nixosConfigurations.vhagar ];
  };

  waycastle = {
    networking.hostName = "waycastle";
    deployment = { targetHost = "172.19.87.1"; };
    imports = [ cell.nixosConfigurations.waycastle ];
  };

}
