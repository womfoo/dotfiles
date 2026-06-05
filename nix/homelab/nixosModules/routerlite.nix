{
  config,
  lib,
  pkgs,
  ...
}:
let
  baseConf = {
    wan = {
      interface = "eth0";
    };
    wireless = {
      ssid = "tatsulok";
      interface = "wlu1";
      ip = "172.19.88.1";
      subnet = "172.19.88.0";
      dhcpLowerRange = "172.19.88.50";
      dhcpUpperRange = "172.19.88.99";
    };
    passwordFile = pkgs.writeTextFile {
      name = "password.lol";
      text = ''
        00:00:00:00:00:00 REDACTED
        yo:ur:ea:ll:yt:hi nkitshouldbehere
      '';
    };
  };
in
with lib;
{
  options = {
    services.routerlite = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to run this module.
        '';
      };
      config = mkOption {
        type = types.attrs;
        default = { };
      };
    };
  };

  config =
    let
      cfg = config.services.routerlite;
      finalConf = lib.recursiveUpdate baseConf cfg.config;
    in
    mkIf cfg.enable {

      networking = {
        useNetworkd = true;
        useDHCP = false;
        interfaces."${finalConf.wan.interface}" = lib.mkForce { };
      };

      systemd.network.enable = true;
      systemd.network.networks."10-${finalConf.wan.interface}" = {
        name = finalConf.wan.interface;
        DHCP = "yes";
      };

      networking.firewall.logRefusedPackets = true;
      networking.firewall.logRefusedConnections = true;

      systemd.network.networks."10-lan" = {
        name = finalConf.wireless.interface;
        networkConfig = {
          DHCPServer = true;
          Address = "${finalConf.wireless.ip}/24";
          IPMasquerade = "ipv4";
        };

        dhcpServerConfig = {
          EmitDNS = true;
          DNS = "8.8.8.8";
          PoolOffset = 100;
          PoolSize = 20;
        };
      };

      networking = {
        nat = {
          enable = true;
          externalInterface = "${finalConf.wan.interface}";
          internalIPs = [
            (finalConf.wireless.subnet + "/24")
          ];
          internalInterfaces = [
            finalConf.wireless.interface
          ];
        };
      };

      services.hostapd = {
        enable = true;
        radios = {
          "${finalConf.wireless.interface}" = {
            band = "2g";
            channel = 11;
            countryCode = "US";
            networks."${finalConf.wireless.interface}" = {
              ssid = finalConf.wireless.ssid;
              authentication = {
                mode = "wpa2-sha1";
                wpaPskFile = finalConf.passwordFile;
              };
            };
          };
        };
      };

      environment.systemPackages = with pkgs; [
        tcpdump
        net-tools
      ];

    };

}
