{ inputs, ... }:
{
  imports = [ inputs.treefmt-nix.flakeModule ];
  perSystem =
    { ... }:
    {
      treefmt.config = {
        flakeCheck = true;
        flakeFormatter = true;
        programs.deadnix.enable = true;
        programs.hclfmt.enable = true;
        programs.mdformat.enable = true;
        programs.mypy.enable = true;
        programs.nixfmt.enable = true;
        programs.shellcheck.enable = true;
        programs.terraform.enable = true;
      };
    };
}
