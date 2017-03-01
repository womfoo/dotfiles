import System.IO
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks   (ToggleStruts(..),avoidStruts,docks,manageDocks)
import XMonad.Hooks.ManageHelpers (doFullFloat,isFullscreen)
import XMonad.Hooks.SetWMName     (setWMName)
import XMonad.Layout.NoBorders    (smartBorders)
import XMonad.Util.EZConfig       (additionalKeys)
import XMonad.Hooks.EwmhDesktops  (fullscreenEventHook)
import XMonad.Util.Run            (spawnPipe,unsafeSpawn)
import Graphics.X11.ExtraTypes.XF86

main = do
  spawn "xscreensaver -nosplash"
  spawn "trayer --height 28 --widthtype request --edge top --align right --transparent true --tint 0 --alpha 64 --monitor primary"
  spawn "emacs"
  spawn "xset -b" -- kill the system bell
  spawn "nm-applet"
  xmproc <- spawnPipe "/run/current-system/sw/bin/xmobar"
  xmonad $ docks defaultConfig
    { modMask            = mod4Mask
    , terminal           = "urxvt"
    , focusedBorderColor = "#cc6666"
    , normalBorderColor  = "#373b41"
    , borderWidth        = 3
    , startupHook        = setWMName "LG3D"
    , handleEventHook    = fullscreenEventHook -- fix chrome fullscreen
    , manageHook         = manageDocks
                           <+> ( isFullscreen --> doFullFloat )
                           <+> manageHook defaultConfig
    , layoutHook         = smartBorders $ avoidStruts $ layoutHook defaultConfig
    , logHook            = dynamicLogWithPP $ xmobarPP
        { ppOutput       = hPutStrLn xmproc
        , ppTitle        = xmobarColor "#b5bd68" "" . shorten 80
        }
    }
    `additionalKeys` [
                      ((mod4Mask, xK_b), sendMessage ToggleStruts)
                     ,((mod4Mask, xK_p), spawn "dmenu_run -fn \"DejaVu Sans Mono:pixelsize=20:style=Book\"")
                     ,((mod4Mask .|. shiftMask , xK_l), unsafeSpawn "xscreensaver-command -lock")
                     ,((0,xF86XK_MonBrightnessUp), spawn "light -A 10")
                     ,((0,xF86XK_MonBrightnessDown), spawn "light -U 10")
                     ,((0,xF86XK_KbdBrightnessUp), spawn "kbdlight up")
                     ,((0,xF86XK_KbdBrightnessDown), spawn "kbdlight down")
                     ,((0,xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume alsa_output.pci-0000_00_1b.0.analog-stereo +1.5%")
                     ,((0,xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume alsa_output.pci-0000_00_1b.0.analog-stereo -1.5%")
                     ,((0,xF86XK_AudioMute), spawn "pactl set-sink-mute alsa_output.pci-0000_00_1b.0.analog-stereo toggle")
                     ]
-- instead of 0 use alsa_output.pci-0000_00_1b.0.analog-stereo
