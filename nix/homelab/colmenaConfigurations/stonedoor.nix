with inputs.lihim.x86_64-linux.lihim.constants.devices;
{
  deployment.targetHost = stonedoor.interfaces.lan.ip;
  imports = [ cell.nixosConfigurations.stonedoor ];
}
