{ nixpkgs ? <nixpkgs>, system ? "armv7l-linux" }:

let
  hostName = "ghost";
  myisoconfig = { pkgs, config, ... }:
    let
      ubootTinkerboard = pkgs.callPackage ./pkgs/uboot/tinkerboard {};
    in
    {

    imports = [
      "${nixpkgs}/nixos/modules/installer/sd-card/sd-image.nix"
      ./shared/builder.nix
      ./shared/gikos-net-telegraf.nix
      ./shared/gikos-kranium.nix
    ];

    boot.loader.generic-extlinux-compatible.enable = true;
    boot.loader.grub.enable = false;
    boot.kernelPackages = pkgs.linuxPackages_5_10;
    # tmpfs makes builds faster than sd
    fileSystems."/tmp" =
      { device = "habilog.gikos.net:/armorydata/${hostName}";
        fsType = "nfs";
        options = ["auto" "nofail" "soft"];
      };
    hardware.bluetooth.enable = true;
    networking.hostName = hostName;
    sound.enable = true;

    environment.systemPackages = with pkgs; [
      aircrack-ng
      cava
      darcs
      dnsutils
      emacs-nox
      ethtool
      git
      hostapd
      htop
      i2c-tools
      iotop
      iperf
      iptraf-ng
      iw
      kubernetes
      libgpiod
      gpio-utils
      mc
      mmc-utils
      ncdu
      nethogs
      nix-top
      pciutils
      pulsemixer
      speedtest-cli
      tcpdump
      telegraf
      wget
      (python39.withPackages(ps: with ps;[
        adafruit-pureio
        pyserial
      ]))
    ];

    services.openssh.enable = true;
    services.journald.extraConfig = ''
      Storage=volatile
    '';

    sdImage = {
      #compressImage = false; #FIXME: does not work yet
      imageBaseName = hostName;
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
  };

  evalNixos = configuration: import "${nixpkgs}/nixos" {
    inherit system configuration;
  };

in { iso = (evalNixos myisoconfig).config.system.build.sdImage; }
