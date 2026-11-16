{
  lib,
  buildGoModule,
}:
buildGoModule rec {
  pname = "updatenixpkgs";
  version = "0.1.0.0";

  src = ./.;

  vendorHash = "sha256-G4c2V7JjYW1b+W4G0wWhW4Jb8uRDc7CZuGCXev01Jsg=";
  proxyVendor = true;

  # vendorHash = null;
}
