{
  inputs = {
    # nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    # nixpkgs.url = "github:NixOS/nixpkgs/9dead5565a9ce7e25d9dfb7230b885bdaf634177";
    nixpkgs.url = "github:NixOS/nixpkgs";
    std.url = "github:divnix/std";

    home-manager = {url = "github:nix-community/home-manager"; inputs.nixpkgs.follows = "nixpkgs";};
    sops-nix = { url ="github:Mic92/sops-nix"; inputs.nixpkgs.follows = "nixpkgs";};
    nur = { url = "github:nix-community/NUR"; };
    hydra = { url = "github:NixOS/hydra"; };

  };

  outputs = inputs:
    (inputs.std.growOn {
      inherit inputs;
      cellsFrom = ./cells;
      organelles = [
       (inputs.std.devshells "devshell")
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
