{
  stdenv,
  fetchFromGitHub,
  ...
}:
stdenv.mkDerivation rec {
  name = "git-recover";
  version = "1.0";
  src = fetchFromGitHub {
    sha256 = "sha256-XVeo0n8gcVDuScDNS9sQaNvoxZm4rfAsgv8iYH7h36M=";
    rev = "v${version}";
    repo = name;
    owner = "ethomson";
  };
  patchPhase = "sed -i 's#/bin/bash#/usr/bin/env bash#' ${name}";
  installPhase = ''
    mkdir -p $out/bin
    cp ${name} $out/bin/
  '';
}
