with import <nixpkgs/lib>;

{ nixpkgs ? { outPath = cleanSource ./..; revCount = 130979; shortRev = "gfedcba"; }
, stableBranch ? false
, supportedSystems ? [ "x86_64-linux" "aarch64-linux" "armv7l-linux"]
, configuration ? {}
}:

with import <nixpkgs/pkgs/top-level/release-lib.nix> { inherit supportedSystems; };

let

  # version = fileContents ../.version;
  # versionSuffix =
  #   (if stableBranch then "." else "pre") + "${toString nixpkgs.revCount}.${nixpkgs.shortRev}";

  versionModule =
    # { system.nixos.versionSuffix = versionSuffix;
    {
      system.nixos.revision = nixpkgs.rev or nixpkgs.shortRev;
    };

  makeModules = module: rest: [ configuration versionModule module rest ];

  makeSdImage =
    { module, system, ... }:

    with import <nixpkgs> { inherit system; };

    hydraJob ((import <nixpkgs/nixos/lib/eval-config.nix> {
      inherit system;
      modules = makeModules module {};
    }).config.system.build.sdImage);

in rec {

  # sd_image = forMatchingSystems [ "armv7l-linux" ] (system: makeSdImage {
  #   module = {
  #       armv7l-linux = ./modules/installer/sd-card/sd-image-armv7l-multiplatform-installer.nix;
  #     }.${system};
  #   inherit system;
  # });

  sd_image_tinkerboard = forMatchingSystems [ "armv7l-linux" ] (system: makeSdImage {
    module = {
      armv7l-linux = ./tinkerboard_sd-card.nix;
    }.${system};
    inherit system;
  });
}
