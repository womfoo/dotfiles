# nix-darwin hosts.
{ inputs, ... }:
{
  flake.darwinConfigurations.tessarion = inputs.darwin.lib.darwinSystem {
    modules = [
      ./tessarion.nix
      { nixpkgs.hostPlatform = "aarch64-darwin"; }
    ];
  };
}
