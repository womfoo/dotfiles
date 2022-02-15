{ pkgs, lib, ... }:
let
  # FIXME: cleanup redundant config with CIDR/subnet parsing
  wanNet      = { interface = "eth1"; };
  wiredNet    = { interface = "eth0";
                  ip             = "172.19.86.1";
                  subnet         = "172.19.86.0";
                  dhcpLowerRange = "172.19.86.100";
                  dhcpUpperRange = "172.19.86.200"; };
  wirelessNet = { ssid = "tatsulok";
                  interface = "wlan0";
                  ip             = "172.19.87.1";
                  subnet         = "172.19.87.0";
                  dhcpLowerRange = "172.19.87.100";
                  dhcpUpperRange = "172.19.87.200"; };
  inventory = import ./inventory.nix { inherit lib; };
  wiredHosts = lib.filterAttrs (name: value: builtins.elem "sydg0" value.tags) inventory;
  dhcpText = ''
    subnet ${wiredNet.subnet} netmask 255.255.255.0 {
      option domain-name-servers ${wiredNet.ip}, 8.8.8.8, 8.8.4.4;
      option routers ${wiredNet.ip};
      range ${wiredNet.dhcpLowerRange} ${wiredNet.dhcpUpperRange};
    }
    subnet ${wirelessNet.subnet} netmask 255.255.255.0 {
      option domain-name-servers ${wirelessNet.ip}, 8.8.8.8, 8.8.4.4;
      option routers ${wirelessNet.ip};
      range ${wirelessNet.dhcpLowerRange} ${wirelessNet.dhcpUpperRange};
    }
  '' + dhcpHosts;
  # FIXME: add wireless hosts
  dhcpHosts = lib.concatStrings (builtins.attrValues (
    builtins.mapAttrs (name: value:
      "host ${name} { hardware ethernet ${value.interfaces.eth0.mac}; fixed-address ${value.interfaces.eth0.ip}; }\n"
    )wiredHosts));
in
{

  networking = {
    # wireless.enable = true; # do not enable, turns on supplicant
    interfaces = {
      "${wiredNet.interface}" = {
        ipv4.addresses = [ { address = wiredNet.ip; prefixLength = 24; } ];
        useDHCP = false;
      };
      "${wirelessNet.interface}" = {
        ipv4.addresses = [ { address = wirelessNet.ip; prefixLength = 24; } ];
        useDHCP = false;
      };
    };
    nat = {
      enable =true;
      externalInterface = "${wanNet.interface}";
      internalIPs = [ (wiredNet.subnet + "/24")
                      (wirelessNet.subnet + "/24") ];
      internalInterfaces = [ wiredNet.interface
                             wirelessNet.interface ];
    };
  };

  # uncomment when testing with NetworkManager present
  #networking.networkmanager.unmanaged = [ wiredNet.interface
  #                                        wirelessNet.interface ];

  services.dhcpd4 = {
    enable = true;
    interfaces =  [ wiredNet.interface wirelessNet.interface ];
    extraConfig = dhcpText;
  };

  services.hostapd = {
    enable = true;
    ssid = wirelessNet.ssid;
    interface = wirelessNet.interface;
    # be less insecure
    extraConfig = ''
      wpa_pairwise=CCMP
    '';
  };

}
