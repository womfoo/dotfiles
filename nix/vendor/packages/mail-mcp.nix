{
  lib,
  fetchFromGitHub,
  rustPlatform,
}:

rustPlatform.buildRustPackage rec {
  pname = "mail-mcp";
  version = "0.4.12";

  src = fetchFromGitHub {
    owner = "tecnologicachile";
    repo = pname;
    rev = "v${version}";
    hash = "sha256-yf97vrpd7ze+xZNKoeUHKMEiSbvVt+sck/Zge/tgGek=";
  };
  # sourceRoot = "${src.name}/bin/gyro2bb";

  cargoHash = "sha256-4+BZvoS1Iu6cNqz++JneqV5bxv1JJbJcObM/tqha/M0=";

}
