{
  inputs,
  cell,
}: {
  default = inputs.std.lib.dev.mkShell {
    commands = with inputs.nixpkgs.pkgs; [
      { package = inputs.std.std.cli.default; }
      { package = alejandra; }
      { package = colmena; }
      { package = darcs; }
      { package = deadnix; }
      { package = git; }
      { package = sops; }
      { package = k9s; }
      { package = kubectl; }
      { package = kubernetes-helm; }
      ];
  } // {
    meta.description  = "basic shell/utils for configuring dotfiles";
  };
}
