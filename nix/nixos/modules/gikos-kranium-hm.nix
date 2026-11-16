{ pkgs, inputs', ... }:
let
  mkTomahawkHost = host: ip: {
    "${host}" = {
      Hostname = ip;
      IdentityFile = "~/Downloads/resbook.pem";
      User = "tomahawk";
      Port = 2200;
      PubkeyAcceptedAlgorithms = "+ssh-rsa";
      HostKeyAlgorithms = "+ssh-rsa";
    };
  };

  kvPairs = inputs'.lihim.cells.lihim.constants.work;
  tomahawk_hosts = pkgs.lib.attrsets.concatMapAttrs (name: value: mkTomahawkHost name value) kvPairs;

  extensions = {
    packages = with pkgs.nur.repos.rycee.firefox-addons; [
      foxyproxy-standard
      privacy-badger
      return-youtube-dislikes
      tree-style-tab
      ublock-origin
      vimium
    ];
  };

in
{
  home-manager.useGlobalPkgs = true;
  home-manager.users.kranium = {
    home.packages = with pkgs; [ deskflow ];
    home.stateVersion = "26.05";
    home.sessionVariables._ZL_MAXAGE = "150000";
    programs = {
      atuin.enable = true;
      atuin.flags = [ "--disable-up-arrow" ];
      # atuin.enableBashIntegration = true; does not make diff direnv first
      bash.enable = true;
      direnv.enable = true;
      firefox = {
        # package = firefox-nightly-bin;
        enable = true;
        # extensions will not work if you dont have profiles defined
        profiles.kranium = {
          id = 0;
          isDefault = true;
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
        settings = {
          merge.conflictstyle = "diff3";
          push.autoSetupRemote = true;
          rerere.enable = true;
          user.email = "kranium@gikos.net";
          user.name = "Kranium Gikos Mendoza";
        };
        lfs.enable = true;
        includes = [
          {
            condition = "gitdir:/home/kranium/git/github.com/womfoo/**";
            contents.user.email = "github@kranium.au";
            contents.user.name = "Kranium Mendoza";
          }
          {
            condition = "gitdir:/home/kranium/respax/git/**";
            contents.user.email = "kranium@cobber.one";
            contents.user.name = "Kranium Mendoza";
            contents.core.sshCommand = "ssh -i ~/.ssh/cobber-2026-vhagar.key -o IdentitiesOnly=yes";
          }
          {
            condition = "gitdir:/armorydata/kranium/respax/git/**";
            contents.user.email = "kranium@cobber.one";
            contents.user.name = "Kranium Mendoza";
            contents.core.sshCommand = "ssh -i ~/.ssh/cobber-2026-vhagar.key -o IdentitiesOnly=yes";
          }
        ];
      };
      mpv = {
        enable = true;
        config = {
          save-position-on-quit = true;
          video-unscaled = "yes"; # downscale-big;
        };
        scripts = with pkgs.mpvScripts; [
          mpris
          vr-reversal
        ];
      };
      ssh.enable = true;
      ssh.serverAliveInterval = 100;
      ssh.settings = tomahawk_hosts;
      ssh.includes = [ "~/.ssh/config.d/*" ];
      tmux = {
        enable = true;
        extraConfig = ''
          set-option -g history-limit 500000
          set-option -g history-limit 500000
          # set-option -g mouse on
          set -g status-bg blue
          set -g status-fg white
        '';
        plugins = with pkgs; [
          tmuxPlugins.yank
        ];
      };
      man.enable = false;
      z-lua.enable = true;
      z-lua.enableBashIntegration = true;
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
    services.ssh-agent.enable = true;
    services.status-notifier-watcher.enable = true;
    services.taffybar.enable = true;
    # xsession.enable is off (xmonad is started by the display manager), so
    # nothing pulls in tray.target on its own
    systemd.user.targets.tray = {
      Unit.PartOf = [ "graphical-session.target" ];
      Install.WantedBy = [ "graphical-session.target" ];
    };
    xdg.configFile."taffybar/taffybar.css".text =
      let
        green = "#b5bd68";
        red = "#cc6666";
        yellow = "#ffffcc";
        fgColor = "#c5c8c6";
        bgColor = "#373b41";
      in
      ''
        @define-color transparent rgba(0, 0, 0, 0);
        @define-color white ${fgColor};

        /* px rather than pt: pt scales with the 4K monitors' DPI and makes the
           bar grow well past barHeight */
        .taffy-window * {
          font-family: "DejaVu Sans Mono";
          font-size: 14px;
          color: ${fgColor};
          /* the GTK theme puts a light shadow under button labels, which
             looks like a white halo on the highlighted window title */
          text-shadow: none;
          -gtk-icon-shadow: none;
        }
        .taffy-box {
          background-color: ${bgColor};
          border-radius: 0;
        }
        /* drop the theme's vertical padding so widgets fit the bar */
        .taffy-window * {
          padding-top: 0;
          padding-bottom: 0;
          margin-top: 0;
          margin-bottom: 0;
          min-height: 0;
        }
        /* the window title is a full-size GTK button unless flattened here;
           red matches xmonad's focusedBorderColor */
        .taffy-window button {
          min-height: 0;
          min-width: 0;
          padding: 0 8px;
          margin: 0;
          border: none;
          border-radius: 0;
          box-shadow: none;
          background-image: none;
          background-color: ${red};
        }
        .taffy-window .windows, .taffy-window .windows * {
          color: #ffffff;
          font-weight: bold;
        }
        /* windowsNew packs the icon and title with no spacing */
        .taffy-window .windows label {
          margin-left: 8px;
        }
        /* menus otherwise get a light background that hides the gray text */
        .taffy-window menu, .taffy-window menuitem, menu, menuitem {
          background-color: ${bgColor};
          background-image: none;
          border: none;
          box-shadow: none;
        }
        .taffy-window menuitem *, menuitem * {
          color: ${fgColor};
          /* menus are popup windows outside .taffy-window, so they otherwise
             fall back to GTK's small default font */
          font-family: "DejaVu Sans Mono";
          font-size: 18px;
        }
        .taffy-window menuitem, menuitem {
          padding: 6px 12px;
        }
        .taffy-window menuitem:hover, menuitem:hover {
          background-color: #4b5058;
        }
        .workspace-label {
          font-size: 14px;
          padding: 0 6px;
        }
        /* white on red like the window title; the stock theme instead darkens
           the active workspace with a translucent black background */
        .active .contents {
          background-color: ${red};
          border-radius: 0;
        }
        .active .workspace-label {
          color: #ffffff;
          font-weight: bold;
        }
        .visible .workspace-label {
          color: ${yellow};
        }
        .urgent .workspace-label {
          color: ${red};
        }
        .active-window {
          color: ${green};
        }
        .low, .critical {
          color: ${red};
        }
      '';
    # taffybar only recompiles when taffybar.hs is newer than its cached
    # binary, and store paths are always dated 1970, so drop the cache
    xdg.configFile."taffybar/taffybar.hs".onChange = ''
      run rm -rf "$HOME/.cache/taffybar"
      run ${pkgs.systemd}/bin/systemctl --user try-restart taffybar.service || true
    '';
    xdg.configFile."taffybar/taffybar.hs".text = ''
      {-# LANGUAGE OverloadedStrings #-}

      import Control.Monad.IO.Class (liftIO)
      import Data.Default (def)
      import Data.List (isPrefixOf, isSuffixOf, sortOn)
      import qualified Data.Text as T
      import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
      import GHC.Conc (getNumProcessors)
      import qualified GI.GdkPixbuf as Pixbuf
      import qualified GI.Gtk as Gtk
      import System.Taffybar (dyreTaffybar)
      import System.Taffybar.Context (TaffyIO)
      import System.Taffybar.Hooks (withLogServer, withToggleServer)
      import System.Taffybar.Information.Workspaces.Model (WindowInfo (..), WorkspaceInfo (..), WorkspaceViewState (..), workspaceHasUrgentWindow)
      import System.Taffybar.SimpleConfig
      import System.Taffybar.Util ((<|||>))
      import System.Taffybar.Widget
      import System.Taffybar.Widget.Generic.PollingLabel (pollingLabelNew)
      import qualified System.Taffybar.Widget.Workspaces as Workspaces
      import System.Directory (listDirectory)
      import System.IO (readFile')
      import Text.Printf (printf)

      main :: IO ()
      main = dyreTaffybar $ withLogServer $ withToggleServer $ toTaffybarConfig myConfig

      -- A window with no icon (e.g. st) otherwise keeps showing whatever icon
      -- was previously drawn in its slot, so fall back to a transparent one.
      blankIcon :: Workspaces.WindowIconPixbufGetter
      blankIcon size _ = liftIO $ do
        pixbuf <- Pixbuf.pixbufNew Pixbuf.ColorspaceRgb True 8 size size
        traverse (\p -> Pixbuf.pixbufFill p 0 >> return p) pixbuf

      -- full window titles instead of the default 35 character cut-off
      windowsConfig :: WindowsConfig
      windowsConfig =
        def
          { getActiveLabel = return . maybe "" windowTitle
          , getMenuLabel = return . windowTitle
          , getActiveWindowIconPixbuf =
              Just $ Workspaces.getWindowIconPixbufByClassHints <|||> Workspaces.getWindowIconPixbufFromEWMH <|||> blankIcon
          }

      -- workspace number only, no window icons. The stock builder overlays the
      -- number on the icons and takes its size from them, so with the icons
      -- turned off it collapses to nothing.
      labelOnlyBuilder :: Workspaces.ControllerConstructor
      labelOnlyBuilder cfg wsInfo = do
        label <- liftIO $ Gtk.labelNew Nothing
        _ <- widgetSetClassGI label "workspace-label"
        contents <- liftIO (Gtk.toWidget label) >>= buildContentsBox
        let states = ["active", "visible", "hidden", "empty", "urgent"]
            stateClass ws
              | workspaceState ws == WorkspaceHidden && workspaceHasUrgentWindow ws = "urgent"
              | otherwise = case workspaceState ws of
                  WorkspaceActive -> "active"
                  WorkspaceVisible -> "visible"
                  WorkspaceHidden -> "hidden"
                  WorkspaceEmpty -> "empty"
            update ws = do
              text <- Workspaces.labelSetter cfg ws
              liftIO $ Gtk.labelSetMarkup label (T.pack text)
              let klass = stateClass ws
              updateWidgetClasses contents [klass] (filter (/= klass) states)
              updateWidgetClasses label [klass] (filter (/= klass) states)
        update wsInfo
        return
          Workspaces.WorkspaceWidgetController
            { Workspaces.controllerWidget = contents
            , Workspaces.controllerUpdate = update
            , Workspaces.controllerRefreshIcons = update
            }

      -- plain label so clicking it does nothing (textClockNewWith pops up a calendar)
      clock :: TaffyIO Gtk.Widget
      clock = pollingLabelNew 1 $ T.pack . formatTime defaultTimeLocale fmt <$> getZonedTime
        where
          fmt = "%a %b %_d %Y <span fgcolor='#ee9a00'>%H:%M:%S</span>"

      -- green below low, yellow below high, red from high up (like xmobar's -L/-H)
      colourValue :: Double -> Double -> Double -> String
      colourValue low high v = printf "<span fgcolor='%s'>%3.0f</span>" colour v
        where
          colour
            | v < low = "#b5bd68" :: String
            | v < high = "#ffffcc"
            | otherwise = "#cc6666"

      -- temp*_input files of the coretemp "Core N" sensors, in core order
      coreTempFiles :: IO [FilePath]
      coreTempFiles = do
        hwmons <- map ("/sys/class/hwmon/" ++) <$> listDirectory "/sys/class/hwmon"
        concat <$> mapM fromHwmon hwmons
        where
          fromHwmon dir = do
            name <- concat . lines <$> readFile' (dir ++ "/name")
            if name /= "coretemp"
              then return []
              else do
                labels <- filter (\f -> "temp" `isPrefixOf` f && "_label" `isSuffixOf` f) <$> listDirectory dir
                labelled <- mapM (\f -> (,) <$> (concat . lines <$> readFile' (dir ++ "/" ++ f)) <*> pure (dir ++ "/" ++ takeWhile (/= '_') f ++ "_input")) labels
                return $ map snd $ sortOn (coreNumber . fst) [l | l@(label, _) <- labelled, "Core " `isPrefixOf` label]
          coreNumber :: String -> Int
          coreNumber = read . drop 5

      -- one coloured temperature per physical core, red from 85C (TjMax is 100C)
      coreTemps :: TaffyIO Gtk.Widget
      coreTemps = do
        files <- liftIO coreTempFiles
        pollingLabelNew 5 $ do
          temps <- mapM (fmap ((/ 1000) . read) . readFile') files
          return $ T.pack $ concatMap (colourValue 60 85) temps ++ "\176C"

      -- 1/5/15 minute load averages, yellow from half the threads busy, red when saturated
      loadAverage :: TaffyIO Gtk.Widget
      loadAverage = do
        threads <- liftIO getNumProcessors
        pollingLabelNew 5 $ do
          loads <- map read . take 3 . words <$> readFile' "/proc/loadavg"
          return $ T.pack $ unwords $ map (colourLoad (fromIntegral threads)) loads
        where
          colourLoad :: Double -> Double -> String
          colourLoad threads v = printf "<span fgcolor='%s'>%.2f</span>" colour v
            where
              colour
                | v < threads / 2 = "#b5bd68" :: String
                | v < threads = "#ffffcc"
                | otherwise = "#cc6666"

      myConfig :: SimpleTaffyConfig
      myConfig =
        def
          { monitorsAction = usePrimaryMonitor
          , startWidgets =
              [ Workspaces.workspacesNew
                  def
                    { Workspaces.showWorkspaceFn = Workspaces.hideEmpty
                    , Workspaces.widgetBuilder = labelOnlyBuilder
                    }
              , layoutNew def
              , windowsNew windowsConfig
              ]
          , -- packed right to left: first entry ends up at the far right
            endWidgets =
              [ sniTrayNew
              , clock
              , textMemoryMonitorNew "$used$/$total$" 1
              , coreTemps
              , loadAverage
              ]
          , barPosition = Top
          , barHeight = ExactSize 28
          , barPadding = 0
          , widgetSpacing = 12
          }
    '';
    xsession.windowManager.xmonad = {
      enable = true;
      # config = inputs.self + /legacy/.xmonad/xmonad.hs;
      enableContribAndExtras = true;
    };
  };
}
