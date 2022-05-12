{ config, pkgs, lib, ... }:
let
  inventory = import ./inventory.nix { inherit lib; };
  builders = lib.filterAttrs (name: value: builtins.elem "builder" value.tags) inventory;
in
{
  nix.distributedBuilds = true;

  sops.secrets.builder-key = {
    format = "binary";
    sopsFile = ../secrets/builders/builder.key;
    owner = "hydra-queue-runner";
    mode = "0400";
  };

  # FIXME: move to DNS
  # FIXME: generalize this for wifi?
  networking.extraHosts = lib.concatStrings (builtins.attrValues (
    builtins.mapAttrs (name: value: value.interfaces.eth0.ip + " " + name + "\n") builders));

  nix.buildMachines = builtins.attrValues (
    builtins.mapAttrs (name: value: { inherit (value) system maxJobs supportedFeatures mandatoryFeatures ;
                                      hostName = name;
                                      sshUser = "builder";
                                      sshKey = config.sops.secrets.builder-key.path;
                                    }) builders);

  services.hydra = {
    enable = true;
    #debugServer = true;
    hydraURL = "http://localhost:7000"; # externally visible URL
    notificationSender = "hydra@localhost"; # e-mail of hydra service
    # a standalone hydra will require you to unset the buildMachinesFiles list to avoid using a nonexistant /etc/nix/machines
    # buildMachinesFiles = [];
    # you will probably also want, otherwise *everything* will be built from scratch
    #useSubstitutes = true;
    port = 7000; # 3000 default also octoprint
    extraConfig = ''
      max_output_size = ${builtins.toString(4*1024*1024*1024)}
    '';
  };

  system.activationScripts =
    { shame.text =
      ''
        chmod 750 /home/kranium
        chown kranium:hydra /home/kranium
        chown -R kranium:hydra /home/kranium/dotfiles/
        find /home/kranium/dotfiles -type f -exec chmod 740 {} \;
        find /home/kranium/dotfiles -type d -exec chmod 750 {} \;
      '';
    };

  services.nginx.enable = true;
  services.nginx.virtualHosts = {
    "cache.gikos.net"  = { /* forceSSL = true; enableACME = true; */ locations."/" = { proxyPass = "http://127.0.0.1:" + builtins.toString(config.services.nix-serve.port); }; };
    "hydra.gikos.net"  = { /* forceSSL = true; enableACME = true; */ locations."/" = { proxyPass = "http://127.0.0.1:" + builtins.toString(config.services.hydra.port); }; };
  };

  sops.secrets.cache-gikos-net-nix-private-key = { };

  services.nix-serve = {
    enable = true;
    port = 6000; # 5000 default is also octoprint
    secretKeyFile = config.sops.secrets.cache-gikos-net-nix-private-key.path;
  };
}
