{ config, pkgs, ... }:
{
  ## Everything below is generated from nixos-in-place; modify with caution!
  boot.kernelParams = ["boot.shell_on_fail"];
  boot.loader.grub.device = "/dev/vda";
  boot.loader.grub.storePath = "/nixos/nix/store";
  boot.initrd.supportedFilesystems = [ "ext4" ];
  boot.initrd.postDeviceCommands = ''
    mkdir -p /mnt-root/old-root ;
    mount -t ext4 /dev/vda1 /mnt-root/old-root ;
  '';
  fileSystems = {
    "/" = {
      device = "/old-root/nixos";
      fsType = "none";
      options = [ "bind" ];
    };
    "/old-root" = {
      device = "/dev/vda1";
      fsType = "ext4";
    };
  };
  
    ## Digital Ocean networking setup; manage interfaces manually
    networking.useDHCP = false;

    systemd.services.setup-network = {
      wantedBy = [ "multi-user.target" ];
      after = [ "network-pre.target" ];
      path = [ pkgs.gawk pkgs.iproute pkgs.openresolv ];
      serviceConfig = {
        ExecStart = "${pkgs.bash}/bin/bash /etc/nixos-in-place/setup-network";
      };
    };
  
}
