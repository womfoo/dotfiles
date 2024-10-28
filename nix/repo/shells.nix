{ inputs, cell }:
{
  default =
    inputs.std.lib.dev.mkShell {
      packages = [
      ];
      commands = with inputs.nixpkgs.pkgs; [
        { package = age; }
        # { package = darcs; }
        # { package = deadnix; }
        # { package = git; }
        { package = inputs.std.std.cli.default; }
        # { package = k9s; }
        # { package = inputs.cells.vendor.packages.keepassx-22-11; }
        # { package = kubectl; }
        # { package = kubernetes-helm; }
        # { package = terraform; }
        { package = colmena; }
        { package = hledger; }
        { package = inputs.agenix.packages.default; }
        { package = stow; }
        { package = nixfmt-rfc-style; }
        # { package = terraform-backend-git; }
      ];
      devshell.startup.stow_legacy_configs.text = ''
        stow --dir=legacy --target=$HOME .
      '';
    }
    // {
      meta.description = "basic shell/utils for configuring dotfiles";
    };
}
