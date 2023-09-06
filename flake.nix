{
  inputs = {
    home-manager = {url = "github:nix-community/home-manager"; inputs.nixpkgs.follows = "nixpkgs";};
    # nixpkgs.url = "github:NixOS/nixpkgs/22.11";

# unstable built 22 sep was
# [kranium@arvoI2c-2:~]$ uname -a
# Linux arvoI2c-2 6.1.21 #1-NixOS SMP Tue Jan  1 00:00:00 UTC 1980 aarch64 GNU/Linux

    #nixpkgs.url = "github:NixOS/nixpkgs/22.11";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # nixpkgs.url = "github:NixOS/nixpkgs/bb31220cca6d044baa6dc2715b07497a2a7c4bc7";
    # nixpkgs.url = "github:NixOS/nixpkgs/SOMEHASH";
    # nixpkgs.url = "path:/home/kranium/git/github.com/womfoo/nixpkgs";
    nur = { url = "github:nix-community/NUR"; };
    sops-nix = { url ="github:Mic92/sops-nix"; inputs.nixpkgs.follows = "nixpkgs";};
    std.url = "github:divnix/std";
    # nixos-hardware = {url = "github:womfoo/nixos-hardware/add-pi-4-sense-v1-overlay"; };
    nixos-hardware = {url = "github:womfoo/nixos-hardware/add-ov5647-overlay"; };
    # nixos-hardware = {url = "github:womfoo/nixos-hardware/1b1a90f4c7f67dc8634badc5b731a7e25516037c";};
    # nixos-hardware = {url = "github:womfoo/nixos-hardware"; };
    std.inputs.devshell.follows = "devshell";
    devshell.url = "github:numtide/devshell";
  };

  outputs = inputs:
    (inputs.std.growOn {
      inherit inputs;
      cellsFrom = ./cells;
      # cellBlocks = [
      cellBlocks = with inputs.std.blockTypes; [
        # Development Environments
        (devshells "devshell")
        (installables "packages")
        (data "modules")
      ];
    }
    {
      devShells = inputs.std.harvest inputs.self [ "automation" "devshell" ];

      nixosConfigurations.silverspark = inputs.nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";
        modules =
          [
            { nixpkgs.overlays = [ inputs.nur.overlay ]; }
            ./nixos/silverspark/configuration.nix
            inputs.home-manager.nixosModule
            inputs.sops-nix.nixosModules.sops
          ];
      };

      nixosConfigurations.vhagar = inputs.nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";
        modules =
          [
            { nixpkgs.overlays = [ inputs.nur.overlay ]; }
            ./nixos/vhagar/configuration.nix
            inputs.home-manager.nixosModule
            inputs.sops-nix.nixosModules.sops
            { environment.systemPackages =
                [
                  # inputs.self.x86_64-linux.vendor.packages.keepassx # qmake4hook gone
                  inputs.self.x86_64-linux.vendor.packages.openlens
                ];
            }
          ];
      };

      nixosConfigurations.stockwell = inputs.nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";
        modules =
          [
            { nixpkgs.overlays = [ inputs.nur.overlay ]; }
            ./nixos/stockwell/configuration.nix
            inputs.home-manager.nixosModule
            inputs.sops-nix.nixosModules.sops
          ];
      };

      nixosConfigurations.habilog = inputs.nixpkgs.lib.nixosSystem rec {
        system = "aarch64-linux";
        modules =
          [
            { nixpkgs.overlays = [ inputs.nur.overlay ]; }
            ./nixos/habilog/configuration.nix
            inputs.sops-nix.nixosModules.sops
          ];
      };

      nixosModules.arvo_loc = {
          boot.loader.generic-extlinux-compatible.enable = false;
          boot.loader.grub.enable = false;
          boot.initrd.availableKernelModules = [ "xhci_pci" ];
          boot.initrd.kernelModules = [ ];
          # boot.kernelPackages = pkgs.linuxPackages_rpi3;
          boot.kernelModules = [ ];
          boot.extraModulePackages = [ ];



          fileSystems."/" =
            { device = "/dev/disk/by-uuid/44444444-4444-4444-8888-888888888888";
              fsType = "ext4";
            };

          swapDevices = [ ];

      };

      nixosModules.arvo =
        { config, lib, pkgs, ... }:
        {
          # hardware.i2c.enable = true;
          # hardware.raspberry-pi."4" =  {
          #    i2c1.enable = true;
          #  };

          imports = [
            ./nixos/shared/common.nix
            ./nixos/shared/gikos-kranium.nix
            # (inputs.nixos-hardware + "/raspberry-pi/4/i2c.nix")
          ];

          # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
          # (the default) this is the recommended approach. When using systemd-networkd it's
          # still possible to use this option, but it's recommended to use it in conjunction
          # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
          networking.useDHCP = lib.mkDefault true;
          # networking.interfaces.end0.useDHCP = lib.mkDefault true;
          # networking.interfaces.wlan0.useDHCP = lib.mkDefault true;

          # boot = {

          #   loader.raspberryPi.enable = true;
          #   loader.raspberryPi.version = 3;
          #   # disable internal sound card and vc4 gpu
          #   # blacklistedKernelModules = ["snd_bcm2835" "vc4"];
          #   # # enable i2c and rtc modules
          #   # kernelModules = ["i2c-dev" "i2c_bcm2708" "rtc_ds1307"];
          #   # kernelPackages = pkgs.linuxKernel.packages.linux_rpi3;
          #   # # kernelPackages = pkgs.linuxPackages_rpi3;
          #   # initrd.availableKernelModules = [
          #   #   "usbhid"
          #   #   "usb_storage"
          #   #   "pcie_brcmstb" # required for the pcie bus to work
          #   #   "reset-raspberrypi" # required for vl805 firmware to load
          #   # ];
          # };

          nixpkgs.hostPlatform = lib.mkDefault "aarch64-linux";

          system.stateVersion = "23.05";
          powerManagement.cpuFreqGovernor = lib.mkDefault "ondemand";

          nixpkgs.config.allowBroken = true;
          hardware.enableRedistributableFirmware = lib.mkForce false;
          # hardware.enableRedistributableFirmware = true;

          hardware.firmware = [ pkgs.firmwareLinuxNonfree ];

          # hardware.firmware = [ pkgs.raspberrypiWirelessFirmware ];
          networking.hostName = "arvoI2c-66";
          networking.wireless.networks = {
            tatsulok.psk = "pbhN0JxUP7bG";
          };
          users.users.root = {
            hashedPassword = "$y$j9T$3MIO0ftE13E4ASMpK8S9i/$zA9w/cB9gkJmBkD.tlt/wsPreSPXQRwOD.kgU2O8pK7";
          };
          environment.systemPackages =
            with pkgs; [ i2c-tools
                         git
                         vim
                         motion
                         dtc
                         gpio-utils
                         v4l-utils
                         ffmpeg-full
                         libraspberrypi
                         (python3.withPackages (
                           ps: with ps; with python3Packages; [
                             # pip
                             # pillow
                             # numpy
                             # inputs.self.aarch64-linux.vendor.packages.rtimu
                             # inputs.self.aarch64-linux.vendor.packages.python-rtimu
                             inputs.self.aarch64-linux.vendor.packages.python-sense-hat
                             rpi-gpio2
                             smbus2
                             rplcd
                             wiringpi
                           ]
                         ))
                         (haskellPackages.ghcWithPackages (self:
                           with haskellPackages;
                           with pkgs.haskell.lib; [
                             pi-lcd
                           ]))
                       ];
          nixpkgs.overlays = [
            (import ./nixos/shared/overlay-aarch64.nix)
          ];
        };

      nixosConfigurations.arvo_sd = inputs.nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";
        modules = [
          (inputs.nixpkgs + /nixos/modules/installer/sd-card/sd-image-aarch64.nix)
          # (inputs.nixpkgs + /nixos/modules/installer/sd-card/sd-image-raspberrypi.nix)
          ({config, lib, ... }: {


            boot.loader.grub.enable = false;

            boot.loader.generic-extlinux-compatible.enable = lib.mkForce true ;
            # boot.loader.raspberryPi.enable = true;
            boot.loader.raspberryPi.uboot.enable = true;
            boot.loader.raspberryPi.version = 3;
            boot.loader.raspberryPi.firmwareConfig = ''
             dtparam=i2c_arm=on
             start_x=1
           '';
            # boot.loader.generic-extlinux-compatible.populateCmd = lib.mkForce ""; # wtf

            # sdImage = {
            #   firmwareSize = 128;
            #   firmwarePartitionName = "NIXOS_BOOT";
            #   # This is a hack to avoid replicating config.txt from boot.loader.raspberryPi
            #   # populateFirmwareCommands =
            #   #   "${config.system.build.installBootLoader} ${config.system.build.toplevel} -d ./firmware";
            #   # # As the boot process is done entirely in the firmware partition.
            #   # populateRootCommands = "";
            # };

            # fileSystems."/boot/firmware" = {
            #   # This effectively "renames" the loaOf entry set in sd-image.nix
            #   mountPoint = "/boot";
            #   neededForBoot = true;
            # };



          })
          { sdImage.compressImage = false; }
          inputs.self.nixosModules.arvo

          ];
        };

      nixosConfigurations.pi2 = inputs.nixpkgs.lib.nixosSystem {
        system = "armv7l-linux";
        modules = [
          ./nixos/shared/common.nix
          ./nixos/shared/gikos-kranium.nix
          (inputs.nixpkgs + /nixos/modules/installer/sd-card/sd-image-armv7l-multiplatform.nix)
          { nixpkgs.config.allowBroken = true; }
            ({ config, lib, pkgs, ... }:
              {
                hardware.enableRedistributableFirmware = lib.mkForce false;
                hardware.firmware = [ pkgs.firmwareLinuxNonfree ];
                sdImage.compressImage = false;
                networking.hostName = "pi2";
                networking.wireless.networks = {
                  tatsulok.psk = "pbhN0JxUP7bG";
                };
                users.users.root = {
                  hashedPassword = "$y$j9T$3MIO0ftE13E4ASMpK8S9i/$zA9w/cB9gkJmBkD.tlt/wsPreSPXQRwOD.kgU2O8pK7";
                };
                environment.systemPackages =
                  with pkgs; [ i2c-tools
                               git
                               vim
                               (python3.withPackages (
                                 ps: with ps; with python3Packages; [
                                   # pip
                                   # pillow
                                   # numpy
                                   # inputs.self.aarch64-linux.vendor.packages.rtimu
                                   # inputs.self.aarch64-linux.vendor.packages.python-rtimu
                                   inputs.self.aarch64-linux.vendor.packages.python-sense-hat
                                   rpi-gpio2
                                   smbus2
                                   rplcd
                                   wiringpi
                                 ]
                               ))
                             #   (haskellPackages.ghcWithPackages (self:
                             #     with haskellPackages;
                             #     with pkgs.haskell.lib; [
                             #       pi-lcd
                             #     ]))
                             ];
                nixpkgs.overlays = [
                  (import ./nixos/shared/overlay-aarch64.nix)
                ];
              })
          ];
        };

      colmena = {
        meta = {
          nixpkgs = import inputs.nixpkgs {
            system = "x86_64-linux";
          };
        };
        au01 = {
          imports = [ ./nixos/au01/configuration.nix
                      inputs.sops-nix.nixosModules.sops
                    ];
        };
        waycastle = {
          imports = [

                      { deployment.targetHost = "172.19.87.1";}
                      inputs.self.x86_64-linux.homelab.modules.router { services.router.enable = true; }
                      # inputs.self.x86_64-linux.homelab.modules.mygit { services.mygit.enable = true; }
                      ./nixos/waycastle/configuration.nix
                      inputs.sops-nix.nixosModules.sops
                    ];
        };

        morning = {
          imports = [
            { networking.hostName = "morning"; }
                      { nixpkgs.system = "aarch64-linux"; }
                      { nixpkgs.overlays = [
                          (import ./nixos/shared/overlay-aarch64.nix)
                        ];
                      }
                      # { deployment.targetHost = "172.19.87.1";}
                      { deployment.targetHost = "192.168.3.37";}
                      # { deployment.targetUser = "nixos";}
                      ./nixos/morning/configuration.nix
                      # (inputs.nixpkgs + /nixos/modules/installer/sd-card/sd-image-aarch64-installer.nix)
                      { nixpkgs.config.allowBroken = true; }
                      ({ config, lib, pkgs, ... }:
                        {
                          # system.stateVersion = "23.05";
                          environment.systemPackages =
                            with pkgs; [
                              motion
                              i2c-tools
                              v4l-utils
                              libcamera
                                         git
                                         vim
                                         gpio-utils
                                         (python3.withPackages(ps: with ps;[
                                           rpi-gpio2
                                           smbus2
                                           rplcd
                                           wiringpi
                                           inputs.self.aarch64-linux.vendor.packages.python-sense-hat
                                         ]))
                                         (haskellPackages.ghcWithPackages (self:
                                           with haskellPackages;
                                           with pkgs.haskell.lib; [
                                             pi-lcd
                                           ]))
                                       ];
                        })
                      # inputs.nixos-hardware.nixosModules.raspberry-pi-4
                      # {hardware.raspberry-pi."4" =  {
                      #    ov5647.enable = true;
                      #   # sense-hat.enable = true;
                      #   # i2c1.enable = true;
                      # }; }

                    ];
        };

      arvo = {
        imports = [
          { nixpkgs.system = "aarch64-linux"; }
          { deployment.targetHost = "192.168.3.38";}
          { deployment.targetUser = "kranium";}
          inputs.self.nixosModules.arvo
          inputs.self.nixosModules.arvo_loc
          ];
        };
      };

    });
}

# add deployer user

