{
  mkDerivation,
  aeson,
  base,
  bytestring,
  iproute,
  lib,
  network,
}:
mkDerivation {
  pname = "wizkell";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson
    base
    bytestring
    iproute
    network
  ];
  description = "Incomplete Local Wiz Bulb UDP tool";
  license = lib.licenses.bsd0;
  mainProgram = "wizkell";
}
