{ config, pkgs, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ../shared/gikos-kranium.nix
  ];

  boot.loader.grub.device = "/dev/vda";
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;

  networking.hostName = "au01";
  networking.domain = "kranium.net";
  networking.useDHCP = false;
  networking.interfaces.enp1s0.useDHCP = true;

  services.openssh.enable = true;
  services.openssh.gatewayPorts = "yes";
  services.openssh.extraConfig = ''
    AllowTcpForwarding yes
  '';

}
