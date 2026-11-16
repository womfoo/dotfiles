{
  description = "Config";
  inputs = {
    agenix.url = "github:yaxitech/ragenix";
    # agenix.url = "github:ryantm/agenix";
    bombon.url = "github:nikstur/bombon/3e573b2c79b9db469c1d28d046bcaf884bb9954f";
    bombon.inputs.nixpkgs.follows = "nixpkgs";
    colmena.inputs.nixpkgs.follows = "nixpkgs";
    colmena.inputs.stable.follows = "nixpkgs";
    colmena.url = "github:zhaofengli/colmena";
    claude.url = "github:k3d3/claude-desktop-linux-flake";
    claude.inputs.nixpkgs.follows = "nixpkgs";
    darwin.inputs.nixpkgs.follows = "nixpkgs-unstable";
    darwin.url = "github:LnL7/nix-darwin";
    devshell.inputs.nixpkgs.follows = "nixpkgs";
    devshell.url = "github:numtide/devshell";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
    # firefox-nightly.url = "github:nix-community/flake-firefox-nightly";
    # home.url = "github:nix-community/home-manager";
    # home.inputs.nixpkgs.follows = "nixpkgs";
    home.url = "github:nix-community/home-manager/release-26.05";
    home.inputs.nixpkgs.follows = "nixpkgs";
    jetpack-nixos.url = "github:anduril/jetpack-nixos";
    kraniumau.url = "github:womfoo/kranium.au";
    lihim.url = "git+file:///home/kranium/git/github.com/womfoo/lihim";
    # lihim.url = "github:womfoo/fake";
    # nixos-hardware.url = "github:nixos/nixos-hardware";
    nixos-25-11.url = "github:NixOS/nixpkgs/nixos-25.11";
    nixos-25-05.url = "github:NixOS/nixpkgs/nixos-25.05";
    nixos-24-11.url = "github:NixOS/nixpkgs/nixos-24.11";
    nixos-24-05.url = "github:NixOS/nixpkgs/nixos-24.05";
    nixos-22-11.url = "github:NixOS/nixpkgs/nixos-23.11";
    nixos-22-05.url = "github:NixOS/nixpkgs/nixos-22.05";
    nixos.follows = "nixpkgs";
    nixos-hardware.url = "github:womfoo/nixos-hardware?ref=add-pi-4-sense-v1-overlay";
    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    # nixpkgs.url = "github:NixOS/nixpkgs/d233902339c02a9c334e7e593de68855ad26c4cb";
    nur = {
      url = "github:nix-community/NUR";
    };
    srvos.url = "github:nix-community/srvos";
    srvos.inputs.nixpkgs.follows = "nixpkgs";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    # terranix.url = "github:terranix/terranix";
    # terranix.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs =
    { flake-parts, ... }@inputs:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];

      imports = [
        ./nix/darwin
        ./nix/devShells
        ./nix/nixos
        ./nix/packages
        ./nix/terra
        ./nix/treefmt.nix
      ];
    };
  nixConfig = {
    allow-import-from-derivation = "true";
  };

}
