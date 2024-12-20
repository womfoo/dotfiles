{
  description = "Config";
  inputs = {
    agenix.url = "github:yaxitech/ragenix";
    colmena.inputs.nixpkgs.follows = "nixpkgs";
    colmena.inputs.stable.follows = "std/blank";
    colmena.url = "github:zhaofengli/colmena";
    darwin.inputs.nixpkgs.follows = "nixos";
    darwin.url = "github:LnL7/nix-darwin";
    devshell.inputs.nixpkgs.follows = "nixpkgs";
    devshell.url = "github:numtide/devshell";
    firefox-nightly.url = "github:nix-community/flake-firefox-nightly";
    hive.inputs.colmena.follows = "colmena";
    hive.inputs.nixpkgs.follows = "nixpkgs";
    hive.url = "github:divnix/hive";
    home.url = "github:nix-community/home-manager";
    lihim.url = "git+file:///home/kranium/git/github.com/womfoo/lihim";
    nixago.inputs.nixago-exts.follows = "";
    nixago.inputs.nixpkgs.follows = "nixpkgs";
    nixago.url = "github:nix-community/nixago";
    # nixos-hardware.url = "github:nixos/nixos-hardware";
    nixos-24-05.url = "github:NixOS/nixpkgs/nixos-24.05";
    nixos-22-11.url = "github:NixOS/nixpkgs/nixos-23.11";
    nixos.follows = "nixpkgs";
    nixos-hardware = {
      # url = "github:NixOS/nixos-hardware";
      url = "git+file:///home/kranium/git/github.com/womfoo/nixos-hardware"; # patch bee.pkgs.overlays for now
    };
    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs.url = "github:NixOS/nixpkgs/18536bf04cd71abd345f9579158841376fdd0c5a";
    # nixpkgs.url = "git+file:///home/kranium/git/github.com/womfoo/nixpkgs";
    nur = {
      url = "github:nix-community/NUR";
    };
    srvos.url = "github:nix-community/srvos";
    std.inputs.devshell.follows = "devshell";
    std.inputs.nixago.follows = "nixago";
    std.inputs.nixpkgs.follows = "nixpkgs";
    std.url = "github:divnix/std/v0.33.3";
    # terranix.url = "github:terranix/terranix";
    # terranix.inputs.nixpkgs.follows = "nixpkgs";
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

            (functions "secrets")

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
      }
      {
        packages.aarch64-linux.sd-image-dreadfort = self.nixosConfigurations.dreadfort.config.system.build.sdImage;
      };

}
