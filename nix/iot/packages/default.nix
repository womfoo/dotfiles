{
  inputs,
  cell,
}:
let
  inherit (inputs.nixpkgs) haskellPackages;
in
rec {
  wizkell = haskellPackages.callCabal2nix "wizkell" ./wizkell { };
  wizkell-shell = haskellPackages.shellFor {
    buildInputs = with haskellPackages; [
      fourmolu
      cabal-install
    ];
    packages = ps: [ wizkell ];
  };
  faketrunkcombined = pkgs.stdenv.mkDerivation {
    name = "localhost-spfiles";
    src = ../../homelab/fake;
    installPhase = ''
      mkdir -p $out/fakehydra
      cp trunk-combined $out/fakehydra
    '';
  };

}
