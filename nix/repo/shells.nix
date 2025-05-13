{
  inputs,
  cell,
}:
let
  python' = (
    inputs.nixpkgs.python3.withPackages (
      ps:
      with ps;
      with inputs.nixpkgs.python3Packages;
      [
        pulsectl
      ]
    )
  );
in
{
  default =
    with inputs.std.lib.dev;
    mkShell {
      packages = [
        python'
        inputs.nixpkgs.trivy # cant find any
        inputs.nixpkgs.grype # grype -v sbom:./result --add-cpes-if-none
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
        (mkNixago cell.configs.treefmt)
        (mkNixago inputs.std.data.configs.conform)
        (mkNixago inputs.std.data.configs.lefthook)
      ];
      devshell.startup.stow_legacy_configs.text = ''
        stow --dir=legacy --target=$HOME .
      '';
    }
    // {
      meta.description = "basic shell/utils for configuring dotfiles";
    };
}
