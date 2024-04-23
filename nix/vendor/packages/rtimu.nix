{
  lib,
  stdenv,
  fetchFromGitHub,
  cmake,
}:

stdenv.mkDerivation rec {
  pname = "RTIMULib";
  version = "7.2.1";

  src = fetchFromGitHub {
    sha256 = "NCHXXbFWqgR+WVzHLM2HfIjDEctCD9PZN6J+k+7wl24=";
    rev = "V${version}";
    repo = pname;
    owner = "RPi-Distro";
  };

  sourceRoot = "${src.name}/${pname}";

  nativeBuildInputs = [ cmake ];
}
