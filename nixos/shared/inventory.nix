{ lib , ... }:
let
  nodes = {
    ghost       = { system = "armv7l-linux";  maxJobs = 0;  tags = [ "sydg0" "builder" "tinkerboard" ]; supportedFeatures = [ ];                mandatoryFeatures = [ ];                  interfaces.eth0.ip  = "172.19.86.201"; }; # a32b01
    nymeria     = { system = "armv7l-linux";  maxJobs = 0;  tags = [ "sydg0" "builder" "tinkerboard" ]; supportedFeatures = [ ];                mandatoryFeatures = [ ];                  interfaces.eth0.ip  = "172.19.86.202"; }; # a32b02
    lady        = { system = "armv7l-linux";  maxJobs = 0;  tags = [ "sydg0" "builder" "tinkerboard" ]; supportedFeatures = [ ];                mandatoryFeatures = [ ];                  interfaces.eth0.ip  = "172.19.86.203"; }; # a32b03
    greywind    = { system = "armv7l-linux";  maxJobs = 0;  tags = [ "sydg0" "builder" "rpi2" ];        supportedFeatures = [ ];                mandatoryFeatures = [ ];                  interfaces.eth0.ip  = "172.19.86.204"; }; # a32b04
    shaggydog   = { system = "armv7l-linux";  maxJobs = 0;  tags = [ "sydg0" "builder" "rpi2" ];        supportedFeatures = [ ];                mandatoryFeatures = [ ];                  interfaces.eth0.ip  = "172.19.86.205"; }; # a32b05
    summer      = { system = "armv7l-linux";  maxJobs = 0;  tags = [ "sydg0" "builder" "rpi2" ];        supportedFeatures = [ ];                mandatoryFeatures = [ ];                  interfaces.eth0.ip  = "172.19.86.206"; }; # a32b06
    viserion    = { system = "armv7l-linux";  maxJobs = 0;  tags = [ "sydg0" "builder" "xu4" ];         supportedFeatures = [ ];                mandatoryFeatures = [ ];                  interfaces.eth0.ip  = "172.19.86.207"; }; # a32b07
    rhaegal     = { system = "armv7l-linux";  maxJobs = 1;  tags = [ "sydg0" "builder" "hc1" ];         supportedFeatures = [ "big-parallel" ]; mandatoryFeatures = [ "big-parallel" ];   interfaces.eth0.ip  = "172.19.86.208"; }; # a32b08
    habilog     = { system = "aarch64-linux"; maxJobs = 1;  tags = [ "sydg0" "builder" "helios64" ];    supportedFeatures = [ "big-parallel" ]; mandatoryFeatures = [/*"big-parallel"*/]; interfaces.eth0.ip  = "172.19.86.1";   };
    silverspark = { system = "x86_64-linux";                tags = [ "sydg0" "desktop" ];                                                                                                 interfaces.eth0.ip  = "172.19.86.100"; };
    stockwell   = { system = "x86_64-linux";                tags = [ "sydg0" "desktop" "usbboot" ];                                                                                       interfaces.eth0.ip  = "172.19.86.101"; };
  };
  nodes_mac = import ./inventory_secrets.nix;
in lib.recursiveUpdate nodes nodes_mac
