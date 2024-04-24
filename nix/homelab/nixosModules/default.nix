{
  router = import ./router.nix;
  common = import ./common.nix;
  builder = import ./builder.nix;
  desktop-apps = import ./desktop-apps.nix;
  gikos-kranium = import ./gikos-kranium.nix;
  gikos-kranium-hm = import ./gikos-kranium-hm.nix;
  myk3s = import ./myk3s.nix;
}
