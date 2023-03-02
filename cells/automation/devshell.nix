{
  inputs,
  cell,
}: {
  default = inputs.std.lib.dev.mkShell {
    commands = with inputs.nixpkgs.pkgs; [
      { package = alejandra; }
      { package = colmena; }
      { package = darcs; }
      { package = git; }
      { package = sops; }
      ];
  };
}
