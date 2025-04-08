{
  description = "Config";
  inputs = {
    agenix.url = "github:yaxitech/ragenix";
    # agenix.url = "github:ryantm/agenix";
    bombon.url = "github:nikstur/bombon/3e573b2c79b9db469c1d28d046bcaf884bb9954f";
    bombon.inputs.nixpkgs.follows = "nixpkgs";
    colmena.inputs.nixpkgs.follows = "nixpkgs";
    colmena.inputs.stable.follows = "std/blank";
    colmena.url = "github:zhaofengli/colmena";
    claude.url = "github:k3d3/claude-desktop-linux-flake";
    claude.inputs.nixpkgs.follows = "nixpkgs";
    darwin.inputs.nixpkgs.follows = "nixos";
    darwin.url = "github:LnL7/nix-darwin";
    devshell.inputs.nixpkgs.follows = "nixpkgs";
    devshell.url = "github:numtide/devshell";
    # firefox-nightly.url = "github:nix-community/flake-firefox-nightly";
    hive.inputs.colmena.follows = "colmena";
    hive.inputs.nixpkgs.follows = "nixpkgs";
    hive.url = "github:divnix/hive";
    home.url = "github:nix-community/home-manager";
    home.inputs.nixpkgs.follows = "nixpkgs";
    home-24-11.url = "github:nix-community/home-manager/release-24.11";
    home-24-11.inputs.nixpkgs.follows = "nixos-24-11";
    jetpack-nixos.url = "github:womfoo/jetpack-nixos?ref=std-compat";
    kraniumau.url = "github:womfoo/kranium.au";
    # lihim.url = "git+file:///home/kranium/git/github.com/womfoo/lihim";
    lihim.url = "github:womfoo/fake";
    microvm.url = "github:astro/microvm.nix";
    microvm.inputs.nixpkgs.follows = "nixpkgs";
    nixago.inputs.nixago-exts.follows = "";
    nixago.inputs.nixpkgs.follows = "nixpkgs";
    # nixago.url = "github:nix-community/nixago";
    nixago.url = "github:korrat/nixago?ref=korrat/push-zwtrrxzqlrqr"; # https://github.com/nix-community/nixago/pull/61
    # nixos-hardware.url = "github:nixos/nixos-hardware";
    nixos-24-11.url = "github:NixOS/nixpkgs/nixos-24.11";
    nixos-24-05.url = "github:NixOS/nixpkgs/nixos-24.05";
    nixos-22-11.url = "github:NixOS/nixpkgs/nixos-23.11";
    nixos.follows = "nixpkgs";
    nixos-hardware.url = "github:womfoo/nixos-hardware?ref=add-pi-4-sense-v1-overlay";
    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:womfoo/nixpkgs/5461b7f";
    # nixpkgs.url = "github:womfoo/nixpkgs/698214a";
    nur = {
      url = "github:nix-community/NUR";
    };
    srvos.url = "github:nix-community/srvos/a48f731";
    srvos.inputs.nixpkgs.follows = "nixos-24-11";
    std.inputs.devshell.follows = "devshell";
    std.inputs.nixago.follows = "nixago";
    std.inputs.nixpkgs.follows = "nixpkgs";
    std.inputs.microvm.follows = "microvm";
    # std.url = "github:divnix/std";
    std.url = "github:womfoo/std?ref=fix/temp-remove-prettier-toml";
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
            (std.blockTypes.installables "packages" { ci.build = true; })

            (microvms "microvms")

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

            (devshells "shells" { ci.build = true; })
            (nixago "configs")
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
        packages = inputs.std.harvest inputs.self [
          [
            "vendor"
            "packages"
          ]
          [
            "repo"
            "packages"
          ]
        ];
      }
      {
        packages.aarch64-linux.sd-image-dreadfort =
          self.nixosConfigurations.dreadfort.config.system.build.sdImage;
        packages.x86_64-linux.default =
          inputs.bombon.lib.x86_64-linux.buildBom
            self.nixosConfigurations.dreadfort.config.system.build.toplevel
            { };
        nope = inputs.bombon.lib.x86_64-linux.buildBom self.x86_64-linux.repo.shells.default { };
      };
}
