{ config, lib, pkgs, modulesPath, ... }:

let
  ubootTinkerboard = pkgs.callPackage ./pkgs/uboot/tinkerboard {};
in
{
  imports = [
    <nixpkgs/nixos/modules/installer/cd-dvd/sd-image.nix>
    ./shared/builder.nix
    # ./shared/gikos-telegraf.nix
    ./shared/gikos-kranium.nix
    # (modulesPath + "/installer/cd-dvd/sd-image.nix")
  ];

  # boot.loader.raspberryPi.enable = lib.mkForce false;
  # boot.loader.grub.devices = "";
  boot.loader.grub.enable = false;
  
  sdImage.populateFirmwareCommands = "";
  sdImage.populateRootCommands = ''
  '';
  sdImage.postBuildCommands = ''
    dd if=/dev/zero of=$img bs=1k count=1023 seek=1 status=noxfer conv=notrunc
    dd if=${ubootTinkerboard}/u-boot-rockchip-with-spl.bin of=$img seek=64 conv=notrunc
  '';
}
