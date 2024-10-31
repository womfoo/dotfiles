{ pkgs, ... }:
{
  bee.system = "x86_64-linux";
  bee.pkgs = import inputs.nixpkgs {
    overlays = [
      (self: super: {
        efivar = super.efivar.overrideAttrs (
          # yolo
          oldAttrs: {
            NIX_CFLAGS_COMPILE = "-Wno-error=format -Wno-error=int-to-pointer-cast";
          }
        );
      })
    ];
    localSystem = "x86_64-linux";
    crossSystem = "armv7l-linux";
  };

  networking.hostName = "syrax";

  documentation.man.enable = false;
  sdImage.compressImage = false;

  imports = [
    (inputs.nixpkgs + /nixos/modules/installer/sd-card/sd-image-armv7l-multiplatform.nix)
  ];

  # nixpkgs.config.allowBroken = true;

  boot.supportedFilesystems = inputs.nixpkgs.lib.mkForce [
    "ntfs"
    "vfat"
  ];

}
