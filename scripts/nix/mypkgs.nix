# stolen from https://github.com/NixOS/nixpkgs/issues/80379#issuecomment-587231983
# nix-build mypkgs.nix -A <TAB>
{ pkgs ? import ./. {}
, maintainer ? "womfoo"
}: with pkgs.lib;
filterAttrs (name: value:
 (builtins.tryEval value).success &&
 elem maintainers.${maintainer} (value.meta.maintainers or [])
) pkgs
