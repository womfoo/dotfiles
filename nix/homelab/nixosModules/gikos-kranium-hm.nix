{ pkgs, ... }:
let
  extensions = with pkgs.nur.repos.rycee.firefox-addons; [
    foxyproxy-standard
    privacy-badger
    return-youtube-dislikes
    tree-style-tab
    ublock-origin
    vimium
  ];
in

{
  home-manager.useGlobalPkgs = true;
  home-manager.users.kranium = {
    # home.packages = [ pkgs.lolcat ];
    home.stateVersion = "23.11";
    programs = {
      bash.enable = true;
      direnv.enable = true;
      firefox = {
        enable = true;
        # extensions will not work if you dont have profiles defined
        profiles.kranium = {
          id = 0;
          isDefault = true;
          path = "s0h80mj1.default-1471996773737"; # FIXME: generalize outside silverspark
          inherit extensions;
        };
        profiles.work = {
          id = 1;
          inherit extensions;
        };
        profiles.test = {
          id = 2;
          inherit extensions;
        };
      };
      git = {
        enable = true;
        userEmail = "kranium@gikos.net";
        userName = "Kranium Gikos Mendoza";
        extraConfig = {
          push.autoSetupRemote = true;
        };
        lfs.enable = true;
        includes = [
          {
            condition = "gitdir:/home/kranium/git/github.com/input-output-hk/**";
            contents.user.email = "kraniumgikos.mendoza@iohk.io";
            contents.user.signingKey = "30079627378B190345DAEF17A578D4096D011982";
            contents.tag.gpgSign = "true";
            contents.commit.gpgSign = "true";
            # format.signOff = true; # does not work
          }
          {
            condition = "gitdir:/home/kranium/git/github.com/hyperledger/**";
            contents.user.email = "kraniumgikos.mendoza@iohk.io";
            contents.user.signingKey = "30079627378B190345DAEF17A578D4096D011982";
            contents.tag.gpgSign = "true";
            contents.commit.gpgSign = "true";
            # format.signOff = true; # does not work
          }
        ];
      };
      ssh.enable = true;
      man.enable = false;
      z-lua.enable = true;
    };
    manual.manpages.enable = false;
    services.dunst.enable = true;
    services.gpg-agent.enable = true;
    services.gpg-agent.maxCacheTtl = 14400;
    services.gpg-agent.grabKeyboardAndMouse = false;
    # services.gpg-agent.pinentryFlavor = "emacs";
    # services.gpg-agent.pinentryFlavor = "gnome3";
    # services.gpg-agent.pinentryFlavor = "curses";
    # Type: null or one of “curses”, “tty”, “gtk2”, “emacs”, “gnome3”, “qt”

    services.gpg-agent.verbose = true;
    # services.gpg-agent.extraConfig = ''
    #   allow-emacs-pinentry
    # '';
  };

}
