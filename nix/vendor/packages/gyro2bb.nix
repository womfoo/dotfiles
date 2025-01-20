{
  lib,
  fetchFromGitHub,
  rustPlatform,
}:

rustPlatform.buildRustPackage rec {
  pname = "telemetry-parser";
  version = "0.3.0";

  src = fetchFromGitHub {
    owner = "AdrianEddy";
    repo = pname;
    rev = "v${version}";
    hash = "sha256-MRJ/0AOBZBJKAzOwDkK2vxr/ekw0yd/NJgdZT7z+xHo=";
  };

  sourceRoot = "${src.name}/bin/gyro2bb";

  cargoHash = "sha256-NajwEDZ3YSo3G5uW7VCKfdMh4YYYvOdKj0TLCWYoxO4=";

}
