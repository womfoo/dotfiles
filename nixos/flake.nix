{
  inputs = {
    nixpkgs = { url = "github:NixOS/nixpkgs/nixos-unstable"; };
    nixpkgs-stable ={ url = "github:NixOS/nixpkgs/release-21.11";};
    home-manager = {url = "github:nix-community/home-manager"; inputs.nixpkgs.follows = "nixpkgs";};
    sops-nix = { url ="github:Mic92/sops-nix"; inputs.nixpkgs.follows = "nixpkgs";};
    nur = { url = "github:nix-community/NUR"; };
    hydra = { url = "github:NixOS/hydra"; };
  };

  outputs = inputs@{self, nixpkgs, home-manager, sops-nix, nur, hydra, ...}: {
    nixosConfigurations.silverspark = nixpkgs.lib.nixosSystem rec {
      system = "x86_64-linux";
      modules =
        [
          { nixpkgs.overlays = [ nur.overlay ]; }
          ./silverspark/configuration.nix
          home-manager.nixosModule
          sops-nix.nixosModule
      ];
    };
    colmena = {
      meta = {
        nixpkgs = import nixpkgs {
          system = "x86_64-linux";
        };
      };
      au01 = {
        imports = [ ./au01/configuration.nix
                    sops-nix.nixosModule
                  ];
      };
    };
 };
}
