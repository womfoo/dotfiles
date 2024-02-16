{
  vhagar = { pkgs, config, lib, ... }: { imports = [ ./vhagar/hardware-configuration.nix ]; };
  waycastle = { pkgs, config, lib, ... }: { imports = [ ./waycastle/hardware-configuration.nix ]; };
}
