{ inputs, cell }:
{
  default =
    inputs.std.lib.dev.mkShell {
      commands = with inputs.nixpkgs.pkgs; [
        # { package = darcs; }
        # { package = deadnix; }
        # { package = git; }
        { package = inputs.std.std.cli.default; }
        # { package = k9s; }
        # { package = kubectl; }
        # { package = kubernetes-helm; }
        { package = terraform; }
        { package = colmena; }
        { package = hledger; }
        { package = sops; }
        { package = stow; }
        { package = nixfmt-rfc-style; }
        { package = terraform-backend-git; }
      ];
      devshell.startup.stow_legacy_configs.text = ''
        stow --dir=legacy --target=$HOME .
      '';
    }
    // {
      meta.description = "basic shell/utils for configuring dotfiles";
    };
}
