with inputs.lihim.x86_64-linux.lihim.constants.devices;
{
  deployment.targetHost = stonedoor.interfaces.lan.ip;
  deployment.targetUser = "kranium";
  imports = [ cell.nixosConfigurations.stonedoor ];
}
