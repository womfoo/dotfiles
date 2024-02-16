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
  } // {
    meta.description  = "basic shell/utils for configuring dotfiles";
  };
}
