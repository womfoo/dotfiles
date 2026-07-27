{
  lib,
  pkgs,
  config,
  ...
}:
{
  services.clamav.daemon.enable = true;
  services.clamav.updater.enable = true;
}
