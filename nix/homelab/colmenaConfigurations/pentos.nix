with inputs.lihim.x86_64-linux.lihim.constants.devices;
{
  deployment.targetHost = pentos.interfaces.lan.ip;
  deployment.targetUser = "kranium";
  imports = [ cell.nixosConfigurations.pentos ];
}
