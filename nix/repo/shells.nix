{
  inputs,
  cell,
}:
let
  inherit (inputs.std.lib) dev cfg;
in
{
  default =
    inputs.std.lib.dev.mkShell {
      packages = [
      ];
      commands = with inputs.nixpkgs.pkgs; [
        { package = age; }
        # { package = darcs; }
        # { package = deadnix; }
        { package = inputs.std.std.cli.default; }
        # { package = kubectl; }
        # { package = kubernetes-helm; }
        # { package = terraform; }
        { package = colmena; }
        { package = inputs.agenix.packages.default; }
        { package = stow; }
        { package = cell.packages.gen-secrets-nix; }
        { package = cell.packages.updatenixpkgs; }
        { package = inputs.cells.desktop.packages.checkghtoken; }
        { package = oci-cli; }
        { package = opentofu; }
        # { package = terraform-backend-git; }
      ];
      nixago = [
        cell.configs.treefmt
        cell.configs.lefthook
        cell.configs.conform
      ];
      devshell.startup.stow_legacy_configs.text = ''
        stow --dir=legacy --target=$HOME .
      '';
    }
    // {
      meta.description = "basic shell/utils for configuring dotfiles";
    };
}
