{ lib , ... }:
let
  nodes = {
    ghost       = { system = "armv7l-linux";  maxJobs = 0;  tags = [ "sydg0" "builder" "tinkerboard" ]; supportedFeatures = [ ];                mandatoryFeatures = [ ];                  }; # a32b01
    nymeria     = { system = "armv7l-linux";  maxJobs = 0;  tags = [ "sydg0" "builder" "tinkerboard" ]; supportedFeatures = [ ];                mandatoryFeatures = [ ];                  }; # a32b02
    lady        = { system = "armv7l-linux";  maxJobs = 0;  tags = [ "sydg0" "builder" "tinkerboard" ]; supportedFeatures = [ ];                mandatoryFeatures = [ ];                  }; # a32b03
    greywind    = { system = "armv7l-linux";  maxJobs = 0;  tags = [ "sydg0" "builder" "rpi2" ];        supportedFeatures = [ ];                mandatoryFeatures = [ ];                  }; # a32b04
    shaggydog   = { system = "armv7l-linux";  maxJobs = 0;  tags = [ "sydg0" "builder" "rpi2" ];        supportedFeatures = [ ];                mandatoryFeatures = [ ];                  }; # a32b05
    summer      = { system = "armv7l-linux";  maxJobs = 0;  tags = [ "sydg0" "builder" "rpi2" ];        supportedFeatures = [ ];                mandatoryFeatures = [ ];                  }; # a32b06
    viserion    = { system = "armv7l-linux";  maxJobs = 0;  tags = [ "sydg0" "builder" "xu4" ];         supportedFeatures = [ ];                mandatoryFeatures = [ ];                  }; # a32b07
    rhaegal     = { system = "armv7l-linux";  maxJobs = 1;  tags = [ "sydg0" "builder" "hc1" ];         supportedFeatures = [ "big-parallel" ]; mandatoryFeatures = [ "big-parallel" ];   }; # a32b08
    habilog     = { system = "aarch64-linux"; maxJobs = 1;  tags = [ "sydg0" "builder" "helios64" ];    supportedFeatures = [ "big-parallel" ]; mandatoryFeatures = [/*"big-parallel"*/]; };
    silverspark = { system = "x86_64-linux";                tags = [ "sydg0" "desktop" ];                                                                                                 };
    stockwell   = { system = "x86_64-linux";                tags = [ "sydg0" "desktop" "usbboot" ];                                                                                       };
    au01        = { system = "x86_64-linux";                tags = [ "vps" ];                                                                                                             };
  };
  nodes_network = {
    ghost       = { interfaces.eth0.ip  = "172.19.86.201"; }; # a32b01
    nymeria     = { interfaces.eth0.ip  = "172.19.86.202"; }; # a32b02
    lady        = { interfaces.eth0.ip  = "172.19.86.203"; }; # a32b03
    greywind    = { interfaces.eth0.ip  = "172.19.86.204"; }; # a32b04
    shaggydog   = { interfaces.eth0.ip  = "172.19.86.205"; }; # a32b05
    summer      = { interfaces.eth0.ip  = "172.19.86.206"; }; # a32b06
    viserion    = { interfaces.eth0.ip  = "172.19.86.207"; }; # a32b07
    rhaegal     = { interfaces.eth0.ip  = "172.19.86.208"; }; # a32b08
    habilog     = { interfaces.eth0.ip  =   "172.19.86.1";
                    interfaces.wg0 = { ip  = "10.100.0.2"; publicKey = "OeCRZ1VuatQ1rGO7lw1S06so/3dSZfXRTKBx3S61kEY="; }; };
    silverspark = { interfaces.eth0.ip  = "172.19.86.100";
                    interfaces.wg0 = { ip  = "10.100.0.100"; publicKey = "yN3RhSg0wcdGZoqZbrnb4zudQQY/XgYaijyyo22ra2c="; }; };
    stockwell   = { interfaces.eth0.ip  = "172.19.86.101"; };
    au01        = { interfaces.eth0.ip = "149.28.180.243";
                    interfaces.wg0 = { ip  = "10.100.0.1"; publicKey = "ykVVI6YfSDarxLfDwwCrnA7KYNUD3lHyK0QkGFtXIgA="; }; };
  };
  nodes_mac = import ./inventory_secrets.nix;
in builtins.foldl' lib.recursiveUpdate {} [ nodes nodes_network nodes_mac ]
