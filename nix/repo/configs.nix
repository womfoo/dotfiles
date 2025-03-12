{
  inputs,
  cell,
}:
let
  inherit (inputs) nixpkgs;
  inherit (inputs.std.data) configs;
  inherit (inputs.std.lib.dev) mkNixago;
in
{
  conform = (mkNixago configs.conform) { };
  lefthook = (mkNixago configs.lefthook) { };
  prettify = (mkNixago configs.prettify) { };
  treefmt = (mkNixago configs.treefmt) {
    data.formatter.go = {
      command = "${nixpkgs.go}/bin/gofmt";
      options = [ "-w" ];
      includes = [ "*.go" ];
    };
    data.formatter.nix = {
      command = nixpkgs.lib.getExe nixpkgs.nixfmt-rfc-style;
    };
    data.formatter.py = {
      command = "${nixpkgs.black}/bin/black";
      includes = [ "*.py" ];
    };
  };
}
