# basic shell/utils for configuring dotfiles
{ inputs, ... }:
{
  imports = [ inputs.devshell.flakeModule ];
  perSystem =
    {
      pkgs,
      inputs',
      self',
      ...
    }:
    let
      python' = (
        pkgs.python3.withPackages (
          ps:
          with ps;
          with pkgs.python3Packages;
          [
            selenium
            evdev
            pulsectl
          ]
        )
      );

      my_azure-cli =
        with pkgs.azure-cli;
        withExtensions [
          extensions.account
          extensions.reservation
        ];

    in
    {
      devshells.default = {
        packages = [
          python'
          # pkgs.trivy # ftb 2026-03-20
          pkgs.grype # grype -v sbom:./result --add-cpes-if-none
          pkgs.grafana-alloy
          (pkgs.writeShellScriptBin "nuke-prom-alloy" ''
            systemctl stop prometheus
            systemctl stop alloy
            rm -rf /var/lib/prometheus2/*
            rm -rf /var/lib/private/alloy/data-alloy/*
          '')
        ];
        commands = with pkgs.pkgs; [
          { package = age; }
          # { package = darcs; }
          # { package = deadnix; }
          # { package = kubectl; }
          # { package = kubernetes-helm; }
          # { package = terraform; }
          { package = terragrunt; }
          { package = inputs'.colmena.packages.colmena; }
          { package = inputs'.agenix.packages.default; }
          { package = stow; }
          { package = self'.packages.gen-secrets-nix; }
          { package = self'.packages.updatenixpkgs; }
          { package = self'.packages.checkghtoken; }
          { package = self'.packages.scanpdf; }
          { package = self'.packages.wizkell; }
          { package = my_azure-cli; }
          { package = oci-cli; }
          { package = opentofu; }
          { package = haskellPackages.arbtt; }
          { package = self'.packages.openp2s; }
          { package = ansible; }
          # { package = terraform-backend-git; }
        ];
        devshell.startup.stow_legacy_configs.text = ''
          stow --dir=legacy --target=$HOME .
          export TG_TF_PATH=$(which tofu)
        '';
      };
    };
}
