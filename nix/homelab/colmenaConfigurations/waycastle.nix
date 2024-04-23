with inputs.lihim.x86_64-linux.lihim.constants.devices;
{
  deployment = {
    # targetHost = waycastle.interfaces.lan.ip;
    targetHost = "172.19.86.1";
  };
  imports = [ cell.nixosConfigurations.waycastle ];
}
