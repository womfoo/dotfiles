import System.IO
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks   (ToggleStruts(..),avoidStruts,docks,manageDocks)
import XMonad.Hooks.ManageHelpers (doFullFloat,isFullscreen)
import XMonad.Hooks.SetWMName     (setWMName)
import XMonad.Layout.NoBorders    (smartBorders)
import XMonad.Util.EZConfig       (additionalKeys)
import XMonad.Hooks.EwmhDesktops  (ewmh,ewmhFullscreen)
import XMonad.Util.Run            (spawnPipe,unsafeSpawn)
import Graphics.X11.ExtraTypes.XF86

main :: IO ()
main = do
  spawn "xsetroot -cursor_name left_ptr"
  spawn "xsetroot -solid darkgray"
  spawn "xsettingsd"
  spawn "xscreensaver -nosplash"
  spawn "trayer --height 28 --widthtype request --edge top --align right --transparent true --tint 0 --alpha 64 --monitor primary"
  spawn "emacs"
  spawn "xset -b" -- kill the system bell
  spawn "nm-applet"
  spawn "pasystray"
  spawn "parcellite -n"
  -- dunno what broke # redshift -O 6500 # doesnt even reset
  -- spawn "redshift -l -33.84:151.21 -t 6500:3500"
  xmproc <- spawnPipe "xmobar"
  xmonad $ docks $ ewmh $ ewmhFullscreen def
    { modMask            = mod4Mask
    , terminal           = "st -f \"DejaVu Sans Mono:pixelsize=20:style=Book\" tmux"
    , focusedBorderColor = "#cc6666"
    , normalBorderColor  = "#373b41"
    , borderWidth        = 3
    , startupHook        = setWMName "LG3D"
    , manageHook         = manageDocks
                           <+> ( isFullscreen --> doFullFloat )
                           <+> manageHook def
                           <+> ( title =? "ediff" --> doFloat)
    , layoutHook         = smartBorders $ avoidStruts $ layoutHook def
    , logHook            = dynamicLogWithPP $ xmobarPP
        { ppOutput       = hPutStrLn xmproc
        , ppTitle        = xmobarColor "#b5bd68" "" . shorten 80
        }
    }
    `additionalKeys` [
                      ((mod4Mask, xK_b), sendMessage ToggleStruts)
                     ,((mod4Mask, xK_p), spawn "PATH=$PATH:~/bin dmenu_run -fn \"DejaVu Sans Mono:pixelsize=20:style=Book\"")
                     ,((mod4Mask .|. shiftMask , xK_l), unsafeSpawn "xscreensaver-command -lock")
                     ,((0,xF86XK_MonBrightnessUp), spawn "sudo light -A 10")
                     ,((0,xF86XK_MonBrightnessDown), spawn "sudo light -U 10")
                     ,((0,xF86XK_KbdBrightnessUp), spawn "kbdlight up")
                     ,((0,xF86XK_KbdBrightnessDown), spawn "kbdlight down")
                     ,((0,xF86XK_AudioRaiseVolume), spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+")
                     ,((0,xF86XK_AudioLowerVolume), spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-")
                     ,((0,xF86XK_AudioMute), spawn "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle")
                     ,((mod4Mask, xK_c), spawn "find-cursor -c red")
                     ,((mod4Mask, xK_n), spawn "networkmanager_dmenu")
                     ,((mod4Mask, xK_u), spawn "~/bin/sendkeys.sh alias_")
                     ,((mod4Mask, xK_o), spawn "~/bin/sendkeys.sh password")
                     ,((mod4Mask, xK_i), spawn "~/bin/sendkeys.sh totp")
                     ,((mod4Mask .|. shiftMask , xK_t), spawn "st -f \"DejaVu Sans Mono:pixelsize=20:style=Book\"")
                     ]
