{
  # lib,
  buildGoModule,
  fetchFromGitHub,
}:
buildGoModule rec {
  pname = "atc-mi-thermometer-exporter";
  version = "0.0.3";

  src = fetchFromGitHub {
    sha256 = "sha256-OqPFI0Gl94CAqW/DrdTCvC4TZfCNMRO7s3TDJLSw8wg=";
    rev = "v${version}";
    repo = pname;
    owner = "jlevesy";
  };

  vendorHash = "sha256-KWBzD7xyZXScPCJSFxiA7OgdOIBXGraZGNuzqirWjR4=";
}
