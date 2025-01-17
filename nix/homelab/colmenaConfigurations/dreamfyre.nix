let
  inherit (inputs.lihim.lihim.constants.devices) dreamfyre;
in {
  deployment = {
    buildOnTarget = true;
    targetHost = dreamfyre.interfaces.lan.ip;
    targetUser = "kranium";
  };
  imports = [cell.nixosConfigurations.dreamfyre];
}
