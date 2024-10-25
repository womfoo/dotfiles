let
  inherit (inputs.lihim.lihim.constants.devices) dreadfort;
in
{
  deployment = {
    buildOnTarget = true;
    targetHost = dreadfort.interfaces.lan.ip;
    targetUser = "kranium";
  };
  imports = [ cell.nixosConfigurations.dreadfort ];
}
