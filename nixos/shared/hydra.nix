{ pkgs, lib, ... }:
let
  inventory = import ./inventory.nix;
  inventory' = lib.filterAttrs (name: value: builtins.elem "builder" value.tags) inventory;
in
{
  nix.buildMachines = builtins.attrValues (
    builtins.mapAttrs (name: value: { inherit (value) system;
                                      hostName = name;
                                      sshUser = "builder";
                                      sshKey = "/var/lib/hydra/builder-key";
                                    }) inventory');
  services.hydra = {
    enable = true;
    debugServer = true;
    hydraURL = "http://localhost:7000"; # externally visible URL
    notificationSender = "hydra@localhost"; # e-mail of hydra service
    # a standalone hydra will require you to unset the buildMachinesFiles list to avoid using a nonexistant /etc/nix/machines
    # buildMachinesFiles = [];
    # you will probably also want, otherwise *everything* will be built from scratch
    useSubstitutes = true;
    port = 7000;
  };
  # FIMXE: follwed steps in https://nixos.wiki/wiki/Binary_Cache need to move this to sops-nix
  services.nix-serve = {
    enable = true;
    secretKeyFile = "/var/cache-priv-key.pem";
  };
}
