# NixOS: modules, overlays, secrets, hosts and colmena deployment.
#
# Every host in `hosts` becomes a `nixosConfigurations.<name>`; hosts with a
# `deployment` also land in `colmenaHive` (same modules, same specialArgs).
#
# Modules receive these extra arguments:
#   inputs, self   - raw flake inputs / this flake
#   inputs', self' - the same, scoped to the host's system (flake-parts);
#                    `inputs'.<x>.cells` is the std tree of a std-based input
#   secrets        - agenix helpers from ./secrets
{
  inputs,
  self,
  lib,
  withSystem,
  ...
}:
let
  inherit (inputs.lihim.x86_64-linux) lihim pubkeys;
  inherit (lihim.constants) devices;

  # { name = ./dir/name.nix; } for every *.nix file in dir
  importDir =
    dir:
    lib.mapAttrs' (name: _: lib.nameValuePair (lib.removeSuffix ".nix" name) (dir + "/${name}")) (
      lib.filterAttrs (name: type: type == "regular" && lib.hasSuffix ".nix" name) (builtins.readDir dir)
    );

  secrets = import ./secrets {
    inherit lib;
    inherit (pubkeys.constants) admins machines;
  };

  hosts = {
    # sandbox VM for coding agents, run with `nix run .#claudebox`
    claudebox.system = "x86_64-linux";
    dreadfort = {
      system = "aarch64-linux";
      deployment = {
        buildOnTarget = true;
        targetHost = devices.dreadfort.interfaces.lan.ip;
        targetUser = "kranium";
      };
    };
    dreamfyre = {
      system = "aarch64-linux";
      nixpkgs = inputs.jetpack-nixos.inputs.nixpkgs;
    };
    meleys.system = "x86_64-linux";
    pentos = {
      system = "x86_64-linux";
      deployment = {
        buildOnTarget = true;
        targetHost = devices.pentos.interfaces.lan.ip;
        targetUser = "kranium";
      };
    };
    stonedoor = {
      system = "x86_64-linux";
      deployment = {
        targetHost = devices.stonedoor.interfaces.lan.ip;
        targetUser = "kranium";
      };
    };
    vhagar = {
      system = "x86_64-linux";
      deployment = {
        allowLocalDeployment = true;
        buildOnTarget = true;
        targetHost = devices.vhagar.interfaces.lan.ip;
        targetUser = "kranium";
      };
    };
    waycastle = {
      system = "x86_64-linux";
      deployment = {
        # targetHost = devices.waycastle.interfaces.lan.ip;
        targetHost = "172.19.86.1";
        # targetHost = "172.19.87.1";
        targetUser = "kranium";
        # buildOnTarget = true;
      };
    };
  };

  hostDefaults = {
    nixpkgs = inputs.nixpkgs;
  };

  # arguments shared by nixosSystem and colmena
  mkHost =
    name: host':
    let
      host = hostDefaults // host';
    in
    withSystem host.system (
      { inputs', self', ... }:
      host
      // {
        specialArgs = {
          inherit
            inputs
            inputs'
            self
            self'
            secrets
            ;
        };
        modules = [
          ./hosts/${name}
          { nixpkgs.hostPlatform = host.system; }
        ];
      }
    );

  evaluated = lib.mapAttrs mkHost hosts;
  deployable = lib.filterAttrs (_: host: host ? deployment) evaluated;
in
{
  # std-based inputs (lihim, kranium.au): inputs'.<x>.cells = <x>.<system>
  perInput = system: flake: lib.optionalAttrs (flake ? ${system}) { cells = flake.${system}; };

  flake = {
    nixosModules = importDir ./modules;

    overlays = import ./overlays;

    # apps (not packages) so CI doesn't build the VM on every push
    apps.x86_64-linux.claudebox = {
      type = "app";
      program = "${self.nixosConfigurations.claudebox.config.microvm.declaredRunner}/bin/microvm-run";
    };

    # `secrets-nix` is the ragenix rules file, see gen-secrets-nix
    lib = { inherit secrets; };

    nixosConfigurations = lib.mapAttrs (
      _: host:
      host.nixpkgs.lib.nixosSystem {
        inherit (host) specialArgs modules;
      }
    ) evaluated;

    colmenaHive = inputs.colmena.lib.makeHive (
      {
        meta = {
          nixpkgs = inputs.nixpkgs.legacyPackages.x86_64-linux;
          # plain nixpkgs; each host's own nixpkgs.{config,overlays} apply on top
          nodeNixpkgs = lib.mapAttrs (_: host: host.nixpkgs.legacyPackages.${host.system}) deployable;
          nodeSpecialArgs = lib.mapAttrs (_: host: host.specialArgs) deployable;
        };
      }
      // lib.mapAttrs (_: host: {
        inherit (host) deployment;
        imports = host.modules ++ [
          # colmena uses nixpkgs' eval-config.nix directly; add what
          # nixpkgs.lib.nixosSystem would, so nodes match nixosConfigurations
          {
            nixpkgs.flake.source = host.nixpkgs.outPath;
            system.nixos.versionSuffix = ".${
              builtins.substring 0 8 host.nixpkgs.lastModifiedDate
            }.${host.nixpkgs.shortRev}";
            system.nixos.revision = host.nixpkgs.rev;
          }
        ];
      }) deployable
    );

    packages = {
      aarch64-linux.sd-image-dreadfort = self.nixosConfigurations.dreadfort.config.system.build.sdImage;
    };
  };
}
