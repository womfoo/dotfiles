{ config, lib, ... }:
let
  baseConf = {
    # wan = { interface = "eth0"; };
    wan = { interface = "enp0s31f6"; };
    # wired  = {
    #   interface      = "enp0s31f6";
    #   vip            = "172.19.86.1";
    #   ip             = "172.19.86.2";
    #   subnet         = "172.19.86.0";
    #   dhcpLowerRange = "172.19.86.50";
    #   dhcpUpperRange = "172.19.86.99"; };
    wireless = {
      ssid           = "tatsulok";
      interface      = "wlan0";
      ip             = "172.19.87.1";
      subnet         = "172.19.87.0";
      dhcpLowerRange = "172.19.87.50";
      dhcpUpperRange = "172.19.87.99"; };
  };
in
with lib;
{
  options = {
    services.router = {
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
      cfg = config.services.router;
      finalConf = lib.recursiveUpdate baseConf cfg.config;
      # finalConf = baseConf;
    in mkIf cfg.enable {
    networking.firewall.extraCommands = "iptables -A INPUT -p vrrp -j ACCEPT";
    # services.keepalived.enable = true;
    # services.keepalived.vrrpInstances.wired = {
    #   interface = finalConf.wired.interface;
    #   state = "MASTER";
    #   priority = 50;
    #   virtualIps = [{ addr = "${finalConf.wired.vip}/24"; }];
    #   virtualRouterId = 1;
    # };

    services.usbmuxd.enable = true; # for ifuse/ios tethering

    # TODO: use iptables to prio wan for wireguard vs all traffic
    networking.dhcpcd.extraConfig = ''
      interface ${finalConf.wan.interface}
      metric 100
    '';

    networking = {
      # wireless.enable = true; # do not enable, turns on supplicant
      interfaces = {
        # "${finalConf.wired.interface}" = {
        #   ipv4.addresses = [ { address = finalConf.wired.ip; prefixLength = 24; } ];
        #   useDHCP = false;
        # };
        "${finalConf.wireless.interface}" = {
          ipv4.addresses = [ { address = finalConf.wireless.ip; prefixLength = 24; } ];
          useDHCP = false;
        };
      };
      nat = {
        enable = true;
        externalInterface = "${finalConf.wan.interface}";
        internalIPs = [ /* (finalConf.wired.subnet + "/24") */
                        (finalConf.wireless.subnet + "/24") ];
        internalInterfaces = [ /* finalConf.wired.interface */
                               finalConf.wireless.interface ];
      };
    };

    services.kea.dhcp4.enable = true;
    services.kea.dhcp4.settings = {
      interfaces-config = {
        interfaces = [
          # finalConf.wired.interface
          finalConf.wireless.interface
        ];
      };
      lease-database = {
        name = "/var/lib/kea/dhcp4.leases";
        persist = true;
        type = "memfile";
      };
      rebind-timer = 2000;
      renew-timer = 1000;
      subnet4 = [
        {
          pools = [
            {
              pool = "${finalConf.wireless.dhcpLowerRange} - ${finalConf.wireless.dhcpUpperRange}";
            }
          ];
          subnet = "${finalConf.wireless.subnet}/24";
          option-data = [
            {
              name = "routers";
              data = finalConf.wireless.ip;
            }
            {
              name ="domain-name-servers";
              data = "8.8.8.8,8.8.4.4";
            }
          ];
        }
        # {
        #   pools = [
        #     {
        #       pool = "${finalConf.wired.dhcpLowerRange} - ${finalConf.wired.dhcpUpperRange}";
        #     }
        #   ];
        #   subnet = "${finalConf.wired.subnet}/24";
        #   option-data = [
        #     {
        #       name = "routers";
        #       data = finalConf.wired.vip;
        #     }
        #     {
        #       name ="domain-name-servers";
        #       data = "8.8.8.8,8.8.4.4";
        #     }
        #   ];
        # }
      ];
      valid-lifetime = 86400;
    };

    # services.dhcpd4 = {
    #   enable = true;
    #   interfaces = [
    #     finalConf.wiredNet.interface
    #     finalConfg.wireless.interface
    #   ];
    #   extraConfig = dhcpText;
    # };

    services.hostapd = {
      enable = true;
      ssid = finalConf.wireless.ssid;
      interface = finalConf.wireless.interface;
      # be less insecure
      # wpaPassphrase = "rj$;!5&eAw";
      extraConfig = ''
      wpa_pairwise=CCMP
      wpa_psk_file=/tmp/wifipasswords.txt
    '';
    };

    # boot.kernel.sysctl."net.ipv4.ip_nonlocal_bind" = 1; # mkDefault 1;

  };
}
