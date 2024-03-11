{
  inputs,
  cell,
}: {
  default = inputs.std.lib.dev.mkShell {
    commands = with inputs.nixpkgs.pkgs; [
      # { package = darcs; }
      # { package = deadnix; }
      # { package = git; }
      # { package = inputs.std.std.cli.default; }
      # { package = k9s; }
      # { package = kubectl; }
      # { package = kubernetes-helm; }
      { package = alejandra; }
      { package = colmena; }
      { package = hledger; }
      { package = sops; }
      ];
     devshell.startup.decrypt_inventory.text = ''
       sops -d secrets/common/inventory_secrets.nix.sops > secrets/common/inventory_secrets.nix
     '';
  } // {
    meta.description  = "basic shell/utils for configuring dotfiles";
  };
}
