{ pkgs }:

with pkgs.python3Packages;
let
  libblepp = stdenv.mkDerivation rec {
    pname = "libblepp";
    version = "1.1";
    src = pkgs.fetchFromGitHub {
      owner = "edrosten";
      repo = "libblepp";
      rev = "VERSION_1_1";
      sha256 = "XLMk08i5OalIOJWyJteR1aBPMn6mL67wiR4KivOlmSA=";
    };
    buildInputs = with pkgs; [ bluez boost];
  };
  warble = buildPythonPackage rec {
    pname = "warble";
    version = "1.2.6";
    src = fetchPypi {
      inherit pname version;
      sha256 = "Z6YBBnEda/wyrQ/WjouY5ob5APR5+lqbbzmbLolvLas=";
    };
    preConfigure = ''
      rm -rf clibs/warble/deps
      # delete lines from makefile lols
      sed -i '125,129d' clibs/warble/Makefile
      substituteInPlace clibs/warble/Makefile \
        --replace "DEPS_BLEPP:=deps/libblepp" "" \
        --replace "\$(DEPS_BLEPP)/libble++.a" "${libblepp}/lib/libble++.a" \
        --replace "-I\$(DEPS_BLEPP)" "-I${libblepp}/include -I${pkgs.bluez.dev}/include -I${pkgs.boost.dev}/include"
         export NIX_LDFLAGS+=" -L${pkgs.bluez}/lib -L${pkgs.boost.dev}/lib"
    '';
    nativeBuildInputs = with pkgs; [ libblepp bluez boost ];
  };
in
buildPythonPackage rec {
  pname = "metawear";
  version = "1.0.2";
  src = fetchPypi {
    inherit pname version;
    sha256 = "gl9HixhYw9smg4d7wMQHRdV0dxapTu2kLd3ZE0olM3w=";
  };
  propagatedBuildInputs = [
    # libblepp
    warble
    requests
  ];
  postFixup = ''
    rm -rf "$out/lib/${python.libPrefix}/site-packages/mbientlab/__pycache__"
  '';
}
