{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (inputs.lihim.lihim) constants;
in
with lib;
{
  options = {
    services.mywg = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to run this module.
        '';
      };
      host = mkOption {
        type = types.str;
      };
      hostPrivKeyFile = mkOption {
        # type = types.attrs;
        type = types.str;
      };
      makeServer = mkOption {
        type = types.bool;
        default = false;
      };
      peer = mkOption {
        type = types.str;
      };
      # FIXME
      # peers = mkOption {
      #   type = types.listOf types.str;
      #   default = [];
      # };
    };
  };

  config =
    let
      cfg = config.services.mywg;
      hostIp = constants.devices."${cfg.host}".interfaces.wg0.ip;
      getPeer = host: {
        inherit (constants.devices."${host}".interfaces.wg0) publicKey allowedIPs endpoint;
      };
      peers = map getPeer cfg.peers;
    in
    mkIf cfg.enable {
      networking.firewall.allowedUDPPorts = lib.optionals cfg.makeServer [ 51820 ];
      networking.wireguard.interfaces.wg0 = {
        # ips = [ "${hostIp}" "/${if cfg.makeServer then 24 else 32}" ];
        ips = [
          (hostIp + "/" + (if cfg.makeServer then "24" else "32"))
        ];

        # ips = [ ("${getIp cfg.host}" + "/32") ];
        privateKeyFile = cfg.hostPrivKeyFile;

        listenPort = if cfg.makeServer then 51820 else null;
        # FIXME: imp inherit peers;
        peers = [
          (
            let
              iface = constants.devices."${cfg.peer}".interfaces.wg0;
            in
            {
              # inherit iface publicKey;
              publicKey = iface.publicKey;
              allowedIPs =
                if cfg.makeServer then [ "${iface.ip}/32" ] else [ "10.0.0.0/8" ]
              # [ "0.0.0.0/0" ]
              ;
              endpoint = if cfg.makeServer then null else iface.endpoint;
            }
          )
        ];
      };
    };
}
