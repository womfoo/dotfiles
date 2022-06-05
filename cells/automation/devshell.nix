{
  inputs,
  cell,
}: let
  inherit (inputs.std) std;
in {
  default = std.lib.mkShell {
    packages = with inputs.nixpkgs.pkgs; [
      inputs.std.std.cli.default
      alejandra
      colmena
      darcs
      git
      sops
    ];
  };
}
