{
  inputs,
  cell,
}: {
  # https://github.com/NixOS/nixpkgs/pull/209778
  openlens = inputs.nixpkgs.callPackage ./openlens.nix {};
}
