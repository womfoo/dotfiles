{
  inputs,
  cell,
}:
let
  inherit (inputs.nixpkgs) haskellPackages stdenv;
in
rec {
  # wizkell = haskellPackages.callCabal2nix "wizkell" ./wizkell { }; # FIXME: revisit why IFD started to fail
  wizkell = haskellPackages.callPackage ./wizkell { };
  wizkellarmv7l =
    inputs.nixos-24-11.legacyPackages.x86_64-linux.pkgsCross.armv7l-hf-multiplatform.haskellPackages.callPackage
      ./wizkell
      { };
  wizkell-shell = haskellPackages.shellFor {
    buildInputs = with haskellPackages; [
      fourmolu
      cabal-install
    ];
    packages = ps: [ wizkell ];
  };
  faketrunkcombined = stdenv.mkDerivation {
    name = "localhost-spfiles";
    src = ../../homelab/fake;
    installPhase = ''
      mkdir -p $out/fakehydra
      cp trunk-combined $out/fakehydra
    '';
  };

}
