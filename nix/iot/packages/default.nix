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
}
