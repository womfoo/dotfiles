{ pkgs }:

with pkgs.python3Packages;

buildPythonApplication rec {
  pname = "xiaomi_mi_scale";
  version = "0.2.2";

  preBuild = "cd src";

  src = pkgs.fetchFromGitHub {
    owner = "lolouk44";
    repo = pname;
    rev = version;
    sha256 = "S8fe95jBS8H3tGK8NuuO9X91Fp24ayOVMelaLv1MvfU=";
  };
  propagatedBuildInputs = [
    paho-mqtt
    bluepy
    # # libblepp
    # warble
    # requests
  ];
  # postFixup = ''
  #   rm -rf "$out/lib/${python.libPrefix}/site-packages/mbientlab/__pycache__"
  # '';
}
