{
  inputs,
  cell,
}:
let
  inherit (inputs.nixpkgs)
    black
    go
    grafana-alloy
    lib
    nixfmt
    terragrunt
    treefmt
    haskellPackages
    nodePackages
    opentofu
    ;
  alloyFormatWrapper = inputs.std.lib.ops.writeScript {
    name = "treefmt-alloy";
    runtimeInputs = [
      grafana-alloy
    ];
    text = ''
      for filename in "$@"; do
          # Check if the file exists
          if [ -f "$filename" ]; then
              echo "Formatting file: $filename"
              alloy format -w "$filename"
          else
              echo "File not found: $filename"
          fi
      done
    '';
  };
in
{
  treefmt = {
    output = "treefmt.toml";
    format = "toml";
    commands = [
      { package = treefmt; }
      { package = nixfmt; }
      { package = nodePackages.prettier; }
    ];
    data.formatter.alloy = {
      command = "${alloyFormatWrapper}/bin/treefmt-alloy";
      includes = [ "*.alloy" ];
    };
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
    data.formatter.hcl = {
      command = "${terragrunt}/bin/terragrunt";
      options = [
        "hcl"
        "format"
        "--file"
      ];
      includes = [ "*.hcl" ];
    };
    data.formatter.nix = {
      command = lib.getExe nixfmt;
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
    data.formatter.tf = {
      command = "${opentofu}/bin/tofu";
      options = [ "fmt" ];
      includes = [ "*.tf" ];
    };

  };
}
