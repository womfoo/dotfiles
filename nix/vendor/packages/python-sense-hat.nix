{
  lib,
  fetchFromGitHub,
  python3,
  rtimu,
}:
let
  py = python3.override {
    packageOverrides = final: prev: {
      python-rtimu =
        with py.pkgs;
        buildPythonPackage rec {
          pname = "python-rtimu";
          inherit (rtimu) version src;
          propagatedBuildInputs = [ rtimu ];
          sourceRoot = "${src.name}/Linux/python";
        };
    };
  };
in
py.pkgs.buildPythonPackage rec {
  pname = "python-sense-hat";
  version = "2.5.1";
  propagatedBuildInputs = with py.pkgs; [
    pillow
    numpy
    python-rtimu
  ];
  src = fetchFromGitHub {
    owner = "astro-pi";
    repo = pname;
    rev = "v.${version}";
    sha256 = "pFIL9gAWV1l85+LKAGhqVu4Xz6VcZXLEl6TiCivlafA=";
  };
}
