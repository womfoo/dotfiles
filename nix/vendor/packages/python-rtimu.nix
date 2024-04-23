{
  lib,
  fetchFromGitHub,
  python3Packages,
  rtimu,
}:
let
  version = "7.2.1";
in

with python3Packages;
buildPythonPackage rec {
  pname = "python-rtimu";
  inherit version;
  propagatedBuildInputs = [ rtimu ];

  # ModuleNotFoundError: No module named 'RTIMU
  # doCheck = false;

  src = fetchFromGitHub {
    sha256 = "NCHXXbFWqgR+WVzHLM2HfIjDEctCD9PZN6J+k+7wl24=";
    rev = "V${version}";
    repo = pname;
    owner = "RPi-Distro";
  };

  sourceRoot = "${src.name}/Linux/python";
}
