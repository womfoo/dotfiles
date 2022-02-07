{ lib, pkgs, config, ... }: {

  users.extraUsers.builder = {
    # name = "builder";
    uid = 5000;
    # createHome = true;
    # home = "/home/builder";
    # shell = "/run/current-system/sw/bin/bash";
    isNormalUser = true;
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILotkuM4ingr/gPOrTfa1Xc//VJnqqXXJEJWXj4u/sKA"
    ];
  };

  nix.settings.trusted-users = [ "builder" ];
}
