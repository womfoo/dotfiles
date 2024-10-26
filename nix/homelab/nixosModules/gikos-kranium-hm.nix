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
      atuin.enable = true;
      bash.enable = true;
      direnv.enable = true;
      firefox = {
        package = inputs.firefox-nightly.packages.${pkgs.system}.firefox-nightly-bin;
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
      tmux = {
        enable = true;
        extraConfig = ''
          set-option -g history-limit 500000
          set-option -g history-limit 500000
          set -g status-bg blue
          set -g status-fg white
        '';
      };
      man.enable = false;
      xmobar = {
        enable = true;
        extraConfig = let
          green = "#b5bd68";
          red = "#cc6666";
          aqua = "#8abeb7";
          yellow = "#ffffcc";
          orange2 = "#ee9a00";
          fgColor = "#c5c8c6";
          bgColor = "#373b41";
        in ''
          Config {font = "DejaVu Sans Mono"
                 ,iconOffset = 28
                 ,fgColor = "${fgColor}"
                 ,bgColor = "${bgColor}"
                 ,position = TopH 28
                 ,lowerOnStart = True
                 ,commands = [
                   Run MultiCpu ["-t","<total0> <total1> <total2> <total3>","-L","30","-H","60","-h","${red}","-l","${green}","-n","${yellow}","-w","3"] 10
                  ,Run Memory ["-t","<usedratio>%","-H","2048","-L","1024","-h","${red}","-l","${green}","-n","${yellow}"] 10
                  ,Run StdinReader
                  ,Run TopMem [] 60
                  ,Run TopProc [] 30
                  ,Run CpuFreq ["-t","<cpu0> <cpu1> <cpu2> <cpu3>GHz","-L","0","-H","2","-l","${green}","-n","${green}","-h","${red}"] 60
                  ,Run CoreTemp ["-t","<core0> <core1> <core2> <core3>c","-L","40","-H","60","-l","${green}","-n","gray90","-h","${red}"] 60
                  ,Run Date "%a %b %_d %Y <fc=${orange2}>%H:%M:%S</fc>" "date" 10
                  ,Run Network "enp9s0u2u1u2" ["-t <rx>/<tx>Kbs","-S true","-L","0","-H","32","--low","${green}","--normal","${yellow}","--high","${red}"] 10
                  ,Run Battery ["-t","<left>% <timeleft>","-L","50","-H","75","-h","${green}","-n","${yellow}","-l","${red}"] 10
                 ]
                 ,sepChar = "%"
                 ,alignSep = "}{"
                 ,template = "%StdinReader% }{ %multicpu% (%top%) | %cpufreq% | %coretemp% | %enp9s0u2u1u2% | %memory% (%topmem%) | %battery% | %date%       "
                 }
        '';
      };
      z-lua.enable = true;
    };
    manual.manpages.enable = true;
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
    xsession.windowManager.xmonad = {
      enable = true;
      # config = inputs.self + /legacy/.xmonad/xmonad.hs;
      enableContribAndExtras = true;
    };
  };

}
