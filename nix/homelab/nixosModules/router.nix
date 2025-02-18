{
  config,
  lib,
  pkgs,
  ...
}:
let
  baseConf = {
    # wan = { interface = "eth0"; };
    wan = {
      interface = "enp0s31f6";
    };
    wired = {
      interface = "enp0s20f0u3u2"; # usb hub w lan
      # vip            = "172.19.86.1";
      ip = "172.19.86.1";
      subnet = "172.19.86.0";
      dhcpLowerRange = "172.19.86.50";
      dhcpUpperRange = "172.19.86.99";
    };
    wireless = {
      ssid = "tatsulok";
      interface = "wlan0";
      ip = "172.19.87.1";
      subnet = "172.19.87.0";
      dhcpLowerRange = "172.19.87.50";
      dhcpUpperRange = "172.19.87.99";
    };
    passwordFile = "/root/yolopasswords.txt";
  };

  hasInterface =
    ifname: device:
    lib.hasAttrByPath [ "interfaces" ifname "ip" ] device
    && lib.hasAttrByPath [ "interfaces" ifname "mac" ] device;

  inventoryWithInterface = ifname: devs: lib.filterAttrs (_: v: hasInterface ifname v) devs;

  keaReservationFromInt =
    ifname: devs:
    lib.mapAttrsToList (_: v: {
      hw-address = lib.attrsets.getAttrFromPath [ "interfaces" ifname "mac" ] v;
      ip-address = lib.attrsets.getAttrFromPath [ "interfaces" ifname "ip" ] v;
    }) (inventoryWithInterface ifname devs);
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
      inventory = mkOption {
        type = types.attrs;
        default = { };
      };
    };
  };

  config =
    let
      cfg = config.services.router;
      finalConf = lib.recursiveUpdate baseConf cfg.config;
    in
    mkIf cfg.enable {
      networking.useNetworkd = true;
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

      services.avahi = {
        enable = true;
        allowInterfaces = [
          finalConf.wired.interface
          finalConf.wireless.interface
        ];
        reflector = true;
      };

      networking = {
        # wireless.enable = true; # do not enable, turns on supplicant
        interfaces = {
          "${finalConf.wired.interface}" = {
            ipv4.addresses = [
              {
                address = finalConf.wired.ip;
                prefixLength = 24;
              }
            ];
            useDHCP = false;
          };
          "${finalConf.wireless.interface}" = {
            ipv4.addresses = [
              {
                address = finalConf.wireless.ip;
                prefixLength = 24;
              }
            ];
            useDHCP = false;
          };
        };
        nat = {
          enable = true;
          externalInterface = "${finalConf.wan.interface}";
          internalIPs = [
            (finalConf.wired.subnet + "/24")
            (finalConf.wireless.subnet + "/24")
          ];
          internalInterfaces = [
            finalConf.wired.interface
            finalConf.wireless.interface
          ];

          forwardPorts = [
            {
              destination = "172.19.86.101:8581";
              proto = "tcp";
              sourcePort = 48581;
            }
          ];
        };
      };

      environment.systemPackages = with pkgs; [
        dhcpcd
        iw
        libimobiledevice
        wirelesstools
      ];

      services.kea.dhcp4.enable = true;
      services.kea.dhcp4.settings = {
        interfaces-config = {
          interfaces = [
            finalConf.wired.interface
            finalConf.wireless.interface
          ];
          # FIXME: revisit systemd timing
          # DHCPSRV_OPEN_SOCKET_FAIL failed to open socket: the interface eth0 has no usable IPv4 addresses configured
          # DHCPSRV_OPEN_SOCKET_FAIL failed to open socket: the interface wlan0 has no usable IPv4 addresses configured
          # DHCP4_OPEN_SOCKETS_FAILED maximum number of open service sockets attempts: 0, has been exhausted without success
          # DHCPSRV_NO_SOCKETS_OPEN no interface configured to listen to DHCP traffic
          service-sockets-max-retries = 5;
          service-sockets-retry-wait-time = 5000;
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
            id = 1;
            pools = [
              { pool = "${finalConf.wireless.dhcpLowerRange} - ${finalConf.wireless.dhcpUpperRange}"; }
            ];
            subnet = "${finalConf.wireless.subnet}/24";
            option-data = [
              {
                name = "routers";
                data = finalConf.wireless.ip;
              }
              {
                name = "domain-name-servers";
                data = "8.8.8.8,8.8.4.4";
              }
            ];
            reservations = keaReservationFromInt "wlan" cfg.inventory;
          }
          {
            id = 2;
            pools = [ { pool = "${finalConf.wired.dhcpLowerRange} - ${finalConf.wired.dhcpUpperRange}"; } ];
            subnet = "${finalConf.wired.subnet}/24";
            option-data = [
              {
                name = "routers";
                data = finalConf.wired.ip;
                # data = finalConf.wired.vip;
              }
              {
                name = "domain-name-servers";
                data = "8.8.8.8,8.8.4.4";
              }
            ];
            reservations = keaReservationFromInt "lan" cfg.inventory;
          }
        ];
        valid-lifetime = 86400;
      };
      services.hostapd = {
        enable = true;
        radios = {
          "${finalConf.wireless.interface}" = {
            noScan = true;
            countryCode = "AU";
            networks."${finalConf.wireless.interface}" = {
              settings = {
                # chanlist = "1 6 11";
                wpa = "2";
              };
              ssid = finalConf.wireless.ssid;
              authentication = {
                mode = "none";
                # mode = "wpa3-sae-transition";
                wpaPskFile = finalConf.passwordFile;
              };
            };
          };
        };
      };
    };
}
