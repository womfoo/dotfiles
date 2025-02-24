{
  config,
  lib,
  ...
}:
with lib;
{
  options = {
    services.mi-temp-exporter = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to run this module.
        '';
      };
      macs = mkOption {
        type = types.listOf types.str;
        default = [ ];
      };
      package = mkOption {
        type = types.path;
        default = inputs.cells.vendor.packages.atc-mi-thermometer-exporter;
      };
    };
  };
  config =
    let
      cfg = config.services.mi-temp-exporter;
      macArgs =
        ms:
        if builtins.length ms == 0 then
          ""
        else
          builtins.concatStringsSep " " (builtins.map (m: "-allow-device ${m}") ms);
    in
    mkIf cfg.enable {
      systemd.services.mi-temp-exporter = {
        description = "MI Thermometer Exporter";
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          ExecStart = "${cfg.package}/bin/exporter ${macArgs cfg.macs}";
          DynamicUser = true;
          CapabilityBoundingSet = "CAP_NET_ADMIN CAP_NET_RAW";
          AmbientCapabilities = "CAP_NET_ADMIN CAP_NET_RAW";
        };
      };
    };
}
