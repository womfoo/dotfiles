with inputs.lihim.x86_64-linux.lihim.constants.devices; {
  stonedoor = {
    deployment = { targetHost = stonedoor.interfaces.lan.ip; };
    imports = [ cell.nixosConfigurations.stonedoor ];
  };
  vhagar = {
    deployment = { allowLocalDeployment = true; };
    imports = [ cell.nixosConfigurations.vhagar ];
  };
  waycastle = {
    deployment = { targetHost = waycastle.interfaces.lan.ip; };
    imports = [ cell.nixosConfigurations.waycastle ];
  };
}
