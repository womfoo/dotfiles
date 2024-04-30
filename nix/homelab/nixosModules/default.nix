{
  router = import ./router.nix;
  common = import ./common.nix;
  builder = import ./builder.nix;
  desktop-apps = import ./desktop-apps.nix;
  daedalus-db-sync = import ./daedalus-db-sync.nix;
  gikos-kranium = import ./gikos-kranium.nix;
  gikos-kranium-hm = import ./gikos-kranium-hm.nix;
}
