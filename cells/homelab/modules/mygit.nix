{ config, lib, ... }:
with lib;
{
  options = {
    services.mygit = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
      lan = mkOption {
        type = types.str;
        default = "boo";
        example = "boo";
      };
  };

  config = let cfg = config.services.mygit; in mkIf cfg.enable {
    # FIXME: implement
    # services.sourcehut.enable = true;
    # services.sourcehut.services = [ "git" ];
  };
}
