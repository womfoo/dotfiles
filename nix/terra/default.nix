# Terraform JSON configs; `nix build .#terra-cloudflare` -> config.tf.json
{ inputs, ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      packages.terra-cloudflare = pkgs.writeText "config.tf.json" (
        builtins.toJSON (import ./cloudflare.nix { inherit inputs; }).cloudflare
      );
    };
}
