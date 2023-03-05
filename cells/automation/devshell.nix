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
      ];
  } // {
    meta.description  = "basic shell/utils for configuring dotfiles";
  };
}
