{
  inputs,
  cell,
}:
let
  inherit (inputs.nixpkgs)
    black
    go
    lib
    nixfmt-rfc-style
    treefmt
    ;
  inherit (inputs.std.data) configs;
  inherit (inputs.std.lib.dev) mkNixago;
in
{
  # FIXME: why does this not work but treefmt below works
  # conform = (mkNixago configs.conform) { };
  # lefthook = mkNixago (
  #   lib.attrsets.recursiveUpdate configs.lefthook {
  #     data.pre-commit.commands.treefmt.run =
  #       "${lib.getExe treefmt} --fail-on-change {staged_files}";
  #   }
  # );
  prettify = (mkNixago configs.prettify) { };
  treefmt = mkNixago (
    lib.attrsets.recursiveUpdate configs.treefmt {
      commands = [ { package = treefmt; } ];
      data.formatter.go = {
        command = "${go}/bin/gofmt";
        options = [ "-w" ];
        includes = [ "*.go" ];
      };
      data.formatter.nix = {
        command = lib.getExe nixfmt-rfc-style;
      };
      data.formatter.py = {
        command = "${black}/bin/black";
        includes = [ "*.py" ];
      };
    }
  );
}
