with inputs.lihim.x86_64-linux.lihim.constants.devices;
{
  deployment.targetHost = pentos.interfaces.lan.ip;
  deployment.targetUser = "kranium";
  deployment.buildOnTarget = true;
  imports = [ cell.nixosConfigurations.pentos ];
}
