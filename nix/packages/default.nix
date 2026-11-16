# Packages, one attrset per subdirectory, merged into `packages.<system>`.
{ inputs, self, ... }:
{
  perSystem =
    { pkgs, system, ... }:
    let
      args = {
        inherit inputs pkgs system;
        writeScript = import ./write-script.nix { inherit pkgs; };
      };
      vendor = import ./vendor args;
    in
    {
      _module.args.pkgs = import inputs.nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };

      packages =
        vendor
        // import ./desktop (args // { inherit (vendor) eyaml; })
        // import ./iot args
        // import ./repo args;
    };

  flake = {
    packages.x86_64-linux.default =
      inputs.bombon.lib.x86_64-linux.buildBom
        self.nixosConfigurations.dreadfort.config.system.build.toplevel
        { };
    nope = inputs.bombon.lib.x86_64-linux.buildBom self.devShells.x86_64-linux.default { };
  };
}
