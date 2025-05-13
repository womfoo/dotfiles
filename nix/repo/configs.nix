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
    haskellPackages
    nodePackages
    ;
in
{
  treefmt = {
    output = "treefmt.toml";
    format = "toml";
    commands = [
      { package = treefmt; }
      { package = nixfmt-rfc-style; }
      { package = nodePackages.prettier; }
    ];
    data.formatter.go = {
      command = "${go}/bin/gofmt";
      options = [ "-w" ];
      includes = [ "*.go" ];
    };
    data.formatter.haskell = {
      command = "${haskellPackages.fourmolu}/bin/fourmolu";
      options = [
        "--mode"
        "inplace"
      ];
      includes = [ "*.hs" ];
    };
    data.formatter.nix = {
      command = lib.getExe nixfmt-rfc-style;
      includes = [ "*.nix" ];
    };
    data.formatter.py = {
      command = "${black}/bin/black";
      includes = [ "*.py" ];
    };
    data.formatter.prettier = {
      command = lib.getExe nodePackages.prettier;
      includes = [
        "*.css"
        "*.html"
        "*.js"
        "*.json"
        "*.jsx"
        "*.md"
        "*.mdx"
        "*.scss"
        "*.ts"
        "*.yaml"
      ];
    };
  };
}
