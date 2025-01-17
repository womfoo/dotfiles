{
  boot.initrd.availableKernelModules = [
    "xhci_pci"
    "ahci"
    "nvme"
    "usb_storage"
    "sd_mod"
  ];
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.consoleMode = "2";
  fileSystems."/" = {
    device = "/dev/disk/by-uuid/cf7b96d0-487d-44f1-8f07-b2c99cbf00c7";
    fsType = "ext4";
  };
  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/27E1-CBFB";
    fsType = "vfat";
  };
  networking.interfaces.enp0s31f6.useDHCP = true;
  # networking.firewall.allowedTCPPorts = [
  #   48581
  # ];

  virtualisation.oci-containers.containers = {
     homebridge = {
       image = "homebridge/homebridge:2024-10-25";
       ports = [ "192.168.3.100:8581:8581" ];
       # does not show up in netstat or iptables
       # ports = [
       #   # "127.0.0.1:8581:8581"
       #   # "48581"
       #   "8581"
       # ];
       volumes = [
         "/volumes/homebridge:/homebridge"
       ];
       autoStart = true;
       # cmd = [
       #   "--base-url"
       #   "\"/hackagecompare\""
       # ];
     };
   };


}
