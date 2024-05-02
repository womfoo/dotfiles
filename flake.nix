{
  description = "Config";
  inputs = {
    cardano-cli.url = "github:IntersectMBO/cardano-cli/cardano-cli-8.22.0.0";
    cardano-db-sync.url = "github:IntersectMBO/cardano-db-sync";
    cardano-node.url = "github:IntersectMBO/cardano-node/8.9.2";
    cardano-parts.url = "github:input-output-hk/cardano-parts";
    cardano-wallet = {
      # url = "github:cardano-foundation/cardano-wallet/v2024-03-27"; # needs node 8+
      url = "github:cardano-foundation/cardano-wallet/v2023-04-14";
    };
    colmena.inputs.nixpkgs.follows = "nixpkgs";
    colmena.inputs.stable.follows = "std/blank";
    colmena.url = "github:zhaofengli/colmena";
    darwin.inputs.nixpkgs.follows = "nixos";
    darwin.url = "github:LnL7/nix-darwin";
    devshell.inputs.nixpkgs.follows = "nixpkgs";
    devshell.url = "github:numtide/devshell";
    hive.inputs.colmena.follows = "colmena";
    hive.inputs.nixpkgs.follows = "nixpkgs";
    hive.url = "github:divnix/hive";
    home-23-11.url = "github:nix-community/home-manager/release-23.11";
    home.follows = "home-23-11";
    lihim.url = "git+file:///home/kranium/git/github.com/womfoo/lihim";
    nixago.inputs.nixago-exts.follows = "";
    nixago.inputs.nixpkgs.follows = "nixpkgs";
    nixago.url = "github:nix-community/nixago";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    nixos.follows = "nixpkgs";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nur = {
      url = "github:nix-community/NUR";
    };
    std.inputs.devshell.follows = "devshell";
    std.inputs.nixago.follows = "nixago";
    std.inputs.nixpkgs.follows = "nixpkgs";
    std.url = "github:divnix/std/v0.33.0";
    terranix.url = "github:terranix/terranix";
    terranix.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs =
    {
      std,
      hive,
      self,
      ...
    }@inputs:
    let
      myCollect = hive.collect // {
        renamer = _cell: target: "${target}";
      };
    in
    hive.growOn
      {
        inherit inputs;
        nixpkgsConfig = {
          allowUnfree = true;
        };
        cellsFrom = ./nix;
        cellBlocks =
          with std.blockTypes;
          with hive.blockTypes;
          [

            (std.blockTypes.terra "terra" inputs.lihim.x86_64-linux.lihim.constants.tfstate_repo)
            (std.blockTypes.installables "packages")

            (functions "overlays")

            # modules implement
            (functions "nixosModules")
            (functions "homeModules")
            (functions "devshellModules")

            # profiles activate
            (functions "hardwareProfiles")
            (functions "nixosProfiles")
            (functions "homeProfiles")
            (functions "devshellProfiles")

            # suites aggregate profiles
            (functions "nixosSuites")
            (functions "homeSuites")

            # configurations can be deployed
            darwinConfigurations
            nixosConfigurations
            colmenaConfigurations

            (devshells "shells")
          ];
      }
      {
        colmenaHive = myCollect self "colmenaConfigurations";
        nixosConfigurations = myCollect self "nixosConfigurations";
        darwinConfigurations = myCollect self "darwinConfigurations";
        devShells = inputs.std.harvest inputs.self [
          "repo"
          "shells"
        ];
        packages = inputs.std.harvest inputs.self [ "vendor" "packages" ];
        defaultPackage = inputs.std.harvest inputs.self [ "vendor" "packages" "openlens" ];
      };
}
