# Self-hosted GitHub Actions runner for womfoo/dotfiles.
#
# Needs secret `github-runner-token.age`: a fine-grained PAT scoped to
# womfoo/dotfiles with "Administration: Read and write" (lets the runner
# register itself and re-register after restarts). Until that file exists
# this module is a no-op, so evaluation stays green.
#
# Workflows opt in with: runs-on: [self-hosted, nixos, pentos]
{
  lib,
  pkgs,
  config,
  secrets,
  ...
}:
let
  hasToken = secrets ? github-runner-token;
in
{
  imports = lib.optional hasToken secrets.github-runner-token.nixosModule;

  config = lib.mkIf hasToken {
    age.secrets.github-runner-token.owner = "github-runner";

    users.users.github-runner = {
      isSystemUser = true;
      group = "github-runner";
    };
    users.groups.github-runner = { };

    # the runner builds through the host daemon; let it use the host's caches
    nix.settings.trusted-users = [ "github-runner" ];

    services.github-runners.dotfiles = {
      enable = true;
      url = "https://github.com/womfoo/dotfiles";
      tokenFile = secrets.github-runner-token.path config;
      name = config.networking.hostName;
      replace = true;
      user = "github-runner";
      group = "github-runner";
      extraLabels = [
        "nixos"
        config.networking.hostName
        pkgs.stdenv.hostPlatform.system
      ];
      extraPackages = with pkgs; [
        attic-client
        cachix
        gitFull
        jq
        nix
      ];
      # throw away the work dir between jobs
      ephemeral = true;
      serviceOverrides = {
        # keep jobs off the attic/nginx state; still allow nix daemon socket
        ProtectHome = "tmpfs";
      };
    };
  };
}
