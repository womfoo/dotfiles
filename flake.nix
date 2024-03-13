{
  description = "Config";

  inputs = {
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
    home-23-05.url = "github:nix-community/home-manager/release-23.05";
    home.follows = "home-23-05";
    lihim.url = "git+file:///home/kranium/git/github.com/womfoo/lihim";
    nixago.inputs.nixago-exts.follows = "";
    nixago.inputs.nixpkgs.follows = "nixpkgs";
    nixago.url = "github:nix-community/nixago";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    nixos.follows = "nixpkgs";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nur = { url = "github:nix-community/NUR"; };
    std.inputs.devshell.follows = "devshell";
    std.inputs.nixago.follows = "nixago";
    std.inputs.nixpkgs.follows = "nixpkgs";
    std.url = "github:divnix/std";
  };

  outputs = {
    std,
    hive,
    self,
    ...
  } @ inputs:
    hive.growOn {
      inherit inputs;
      nixpkgsConfig = {allowUnfree = true;};
      cellsFrom = ./cells;
      cellBlocks =
        with std.blockTypes;
        with hive.blockTypes; [

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
      colmenaHive = hive.collect self "colmenaConfigurations";
      nixosConfigurations = hive.collect self "nixosConfigurations";
      darwinConfigurations = hive.collect self "darwinConfigurations";
      devShells = inputs.std.harvest inputs.self [ "automation" "shells" ];
    };

}
