{
  pkgs,
  config,
  ...
}:
{
  environment.systemPackages = with pkgs; [
    # avidemux
    # claude-code
    # claude-code-router
    # cura # FTB pynest2d
    dropbox
    # freecad # FTB shiboken2
    github-copilot-cli
    google-chrome
    # (tor-browser-bundle-bin.override { pulseaudioSupport = true; })
    # vbetool # FTB # sudo vbetool dpms off
    # vagrant-25-05
  ];
}
