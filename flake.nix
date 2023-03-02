{
  inputs = {
    home-manager = {url = "github:nix-community/home-manager"; inputs.nixpkgs.follows = "nixpkgs";};
    # nixpkgs.url = "github:NixOS/nixpkgs/22.11";
    # nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    # nixpkgs.url = "path:/home/kranium/git/github.com/womfoo/nixpkgs";
    nixpkgs.url = "github:NixOS/nixpkgs/68196a61c26748d3e53a6803de3d2f8c69f27831"; # unstable (2023-03-02)
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
            inputs.sops-nix.nixosModule
          ];
      };

      nixosConfigurations.vhagar = inputs.nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";
        modules =
          [
            { nixpkgs.overlays = [ inputs.nur.overlay ]; }
            ./nixos/vhagar/configuration.nix
            inputs.home-manager.nixosModule
            inputs.sops-nix.nixosModule
            { environment.systemPackages = [ inputs.self.x86_64-linux.vendor.packages.openlens  ]; }
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
                      inputs.sops-nix.nixosModule
                    ];
        };
      };

    });
}
