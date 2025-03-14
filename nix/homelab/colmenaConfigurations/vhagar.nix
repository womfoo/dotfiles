with inputs.lihim.x86_64-linux.lihim.constants.devices;
{
  deployment = {
    targetHost = vhagar.interfaces.lan.ip;
    targetUser = "kranium";
    allowLocalDeployment = true;
    buildOnTarget = true;
  };
  imports = [ cell.nixosConfigurations.vhagar ];
}
