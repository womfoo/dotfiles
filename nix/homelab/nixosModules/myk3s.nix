{ config, lib, pkgs, ... }:
with lib;
{
  options = {
    services.myk3s = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to run this module.
        '';
      };
      config = mkOption {
        type = types.attrs;
        default = { };
      };
    };
  };

  config =
    let
      cfg = config.services.myk3s;
    in
    mkIf cfg.enable {
      services.k3s.enable = true;
      services.k3s.role = "server";
      # Slightly reduce resource usage
      services.k3s.extraFlags = ''
        --disable=traefik" \
        --server-arg '--kubelet-arg=eviction-hard=imagefs.available<5%,nodefs.available<5%' \
        --server-arg '--kubelet-arg=eviction-minimum-reclaim=imagefs.available=5%,nodefs.available=5%'
    '';

      networking.firewall.trustedInterfaces = [ "cni+" ]; # k3s
      networking.firewall.allowedTCPPorts = [
        6443 # k3s api
        80
        443
      ];
    };
}
