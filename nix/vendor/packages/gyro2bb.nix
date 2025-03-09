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

  useFetchCargoVendor = true;
  cargoHash = "sha256-xlMzHV+4YmjcpXJwdV5gOxemy+LzRK33n+HTiM7e9zE=";

}
