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
        selenium
        evdev
        pulsectl
      ]
    )
  );

  my_azure-cli =
    with inputs.nixpkgs.pkgs.azure-cli;
    withExtensions [
      extensions.account
      extensions.reservation
    ];

in
{
  default =
    with inputs.std.lib.dev;
    mkShell {
      packages = [
        python'
        inputs.nixpkgs.trivy # cant find any
        inputs.nixpkgs.grype # grype -v sbom:./result --add-cpes-if-none
        inputs.nixpkgs.grafana-alloy
      ];
      commands = with inputs.nixpkgs.pkgs; [
        { package = age; }
        # { package = darcs; }
        # { package = deadnix; }
        { package = inputs.std.std.cli.default; }
        # { package = kubectl; }
        # { package = kubernetes-helm; }
        # { package = terraform; }
        { package = terragrunt; }
        { package = inputs.colmena.packages.colmena; }
        { package = inputs.agenix.packages.default; }
        { package = stow; }
        { package = cell.packages.gen-secrets-nix; }
        { package = cell.packages.updatenixpkgs; }
        { package = inputs.cells.desktop.packages.checkghtoken; }
        { package = inputs.cells.desktop.packages.scanpdf; }
        { package = inputs.cells.iot.packages.wizkell; }
        { package = my_azure-cli; }
        { package = oci-cli; }
        { package = opentofu; }
        { package = haskellPackages.arbtt; }
        { package = ansible; }
        # { package = terraform-backend-git; }
      ];
      nixago = [
        (mkNixago cell.configs.treefmt)
        (mkNixago inputs.std.data.configs.conform)
        (mkNixago inputs.std.data.configs.lefthook)
      ];
      devshell.startup.stow_legacy_configs.text = ''
        stow --dir=legacy --target=$HOME .
        export TG_TF_PATH=$(which tofu)
      '';
    }
    // {
      meta.description = "basic shell/utils for configuring dotfiles";
    };
}
