{ config, lib, pkgs, modulesPath, ... }:

let
  ubootTinkerboard = pkgs.callPackage ./pkgs/uboot/tinkerboard {};
in
{
  imports = [
    <nixpkgs/nixos/modules/installer/sd-card/sd-image.nix>
    ./shared/builder.nix
    # ./shared/gikos-telegraf.nix
    ./shared/gikos-kranium.nix
    # (modulesPath + "/installer/cd-dvd/sd-image.nix")
  ];

  # boot.loader.raspberryPi.enable = lib.mkForce false;
  # boot.loader.grub.devices = "";

  boot.loader.generic-extlinux-compatible.enable = true;
  boot.loader.grub.enable = false;
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernelPatches = lib.singleton {
      name = "mainline 5.13 disable ks8851";
      patch = null;
      extraConfig = ''
        KS8851 n
      '';
    };

  # tmpfs makes builds faster than sd
  fileSystems."/tmp" =
    { device = "habilog.gikos.net:/armorydata/${config.sdImage.imageBaseName}";
      fsType = "nfs";
      options = ["auto" "nofail" "soft"];
    };


  networking.hostName = "${config.sdImage.imageBaseName}";

  environment.systemPackages = with pkgs; [
    darcs
    dnsutils
    emacs-nox
    git
    htop
    i2c-tools
    iotop
    iperf
    iptraf-ng
    kubernetes
    mc
    mmc-utils
    ncdu
    nethogs
    nix-top
    speedtest-cli
    tcpdump
    wget
  ];

  services.openssh.enable = true;

  sdImage = {
    #compressImage = false; #FIXME: does not work yet
    imageBaseName = "a32b1";
    populateFirmwareCommands = "";
    populateRootCommands = ''
      mkdir -p ./files/boot
      ${config.boot.loader.generic-extlinux-compatible.populateCmd} -c ${config.system.build.toplevel} -d ./files/boot
    '';
    postBuildCommands = ''
      dd if=/dev/zero of=$img bs=1k count=1023 seek=1 status=noxfer conv=notrunc
      dd if=${ubootTinkerboard}/u-boot-rockchip-with-spl.bin of=$img seek=64 conv=notrunc
    '';
  };

  nixpkgs.overlays = [
    (self: super: {
      haskellPackages = super.haskellPackages.override {
        overrides = haskellSelf: haskellSuper: {
          vector = self.haskell.lib.dontCheck haskellSuper.vector;
          zip-archive = self.haskell.lib.dontCheck haskellSuper.zip-archive;
          hint = self.haskell.lib.dontCheck haskellSuper.hint;
        };
      };
    })

   (self: super: {
     python39Packages = pkgs.python36Packages.override {
       overrides = pythonSelf: pythonSuper: {
         whoosh = pythonSuper.whoosh.overrideAttrs ( z : rec { doCheck=false; doInstallCheck = false;});
         pyflakes = pythonSuper.pyflakes.overrideAttrs( z : rec{ doCheck=false; doInstallCheck = false; } );
       };
     };
   })
  ];

  services.xserver = {
    enable = true;
    desktopManager.xterm.enable = false;
    displayManager.defaultSession = "none+xmonad";
    displayManager.sddm.enable = true;
    windowManager.xmonad.enable = true;
    windowManager.xmonad.enableContribAndExtras = true;
  };

}
