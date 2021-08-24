### https://github.com/NixOS/nixpkgs/issues/128893
{ pkgs, lib, ... }:

{
  system.replaceRuntimeDependencies = lib.singleton {
    original = pkgs.alsa-lib;
    replacement = pkgs.alsa-lib.overrideAttrs (drv: {
      # NOTES:
      #
      # Since the store paths are replaced in the system closure, we can't use
      # "1.2.5.1" here because it would result in a different length.
      #
      # Additionally, the assertion here is to make sure that once version
      # 1.2.5.1 hits the system we get an error and can remove this altogether.
      version = "1.2.X";
      #version = assert pkgs.alsa-lib.version == "1.2.5"; "1.2.X";
      src = pkgs.fetchurl {
        url = "mirror://alsa/lib/${drv.pname}-1.2.5.1.tar.bz2";
        hash = "sha256-YoQh2VDOyvI03j+JnVIMCmkjMTyWStdR/6wIHfMxQ44=";
      };
    });
  };
}
