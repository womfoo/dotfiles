{
  pkgs,
  config,
  ...
}: {
  environment.systemPackages = with pkgs; [
    avidemux
    # cura # FTB pynest2d
    dropbox
    # freecad # FTB shiboken2
    google-chrome
    # (tor-browser-bundle-bin.override { pulseaudioSupport = true; })
    # vbetool # FTB # sudo vbetool dpms off
  ];
}
