{
  inputs = {
    home-manager = {url = "github:nix-community/home-manager"; inputs.nixpkgs.follows = "nixpkgs";};
    # nixpkgs.url = "github:NixOS/nixpkgs/22.11";
    # nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs.url = "github:NixOS/nixpkgs/e603dc5f061ca1d8a19b3ede6a8cf9c9fcba6cdc"; # unstable (2023-06-22)
    # nixpkgs.url = "path:/home/kranium/git/github.com/womfoo/nixpkgs";
    nur = { url = "github:nix-community/NUR"; };
    sops-nix = { url ="github:Mic92/sops-nix"; inputs.nixpkgs.follows = "nixpkgs";};
    std.url = "github:divnix/std";
  };

  outputs = inputs:
    (inputs.std.growOn {
      inherit inputs;
      cellsFrom = ./cells;
      cellBlocks = [
       (inputs.std.devshells "devshell")
       (inputs.std.installables "packages")
       (inputs.std.data "modules")
      ];
    }
    {
      devShell = inputs.std.harvest inputs.self [ "automation" "devshell" "default" ];

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
            { environment.systemPackages = [ inputs.self.x86_64-linux.vendor.packages.openlens  ]; }
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
                      { deployment.targetHost = "172.19.86.2";}
                      inputs.self.x86_64-linux.homelab.modules.router { services.router.enable = true; }
                      # inputs.self.x86_64-linux.homelab.modules.mygit { services.mygit.enable = true; }
                      ./nixos/waycastle/configuration.nix
                      inputs.sops-nix.nixosModules.sops
                    ];
        };

      };

    });
}
