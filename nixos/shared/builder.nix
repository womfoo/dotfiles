{ lib, pkgs, config, ... }: {

  users.extraUsers.builder = {
    # name = "builder";
    uid = 5000;
    # createHome = true;
    # home = "/home/builder";
    # shell = "/run/current-system/sw/bin/bash";
    isNormalUser = true;
    openssh.authorizedKeys.keys = [
      "ecdsa-sha2-nistp521 AAAAE2VjZHNhLXNoYTItbmlzdHA1MjEAAAAIbmlzdHA1MjEAAACFBADIlFNZMOTvaYlqOSkB4m6I0VYVaOcTDRkIiRayhtp4W0OdmNdFkrxG7y7Dw5og6SCkKV1Gp8d/uJKpnOKa6zxElQGJfdYkjFSYdCn1LNUfPSDv9AJ/ujwrWHBfmk/rE74sSoKaYpyLCjrcoFDPoALNMhOaL0l7RY1LCeZSjLhO/I67ng== kranium@silverspark"
    ];
  };

  nix.trustedUsers = [ "builder" ];
}
