import System.IO
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks   (ToggleStruts(..),avoidStruts,manageDocks)
import XMonad.Hooks.ManageHelpers (doFullFloat,isFullscreen)
import XMonad.Hooks.SetWMName     (setWMName)
import XMonad.Layout.NoBorders    (smartBorders)
import XMonad.Util.EZConfig       (additionalKeys)
import XMonad.Hooks.EwmhDesktops  (fullscreenEventHook)
import XMonad.Util.Run            (spawnPipe,unsafeSpawn)
import Graphics.X11.ExtraTypes.XF86

main = do
  xmproc <- spawnPipe "/run/current-system/sw/bin/xmobar"
  xmonad $ defaultConfig
    { modMask            = mod4Mask
    , terminal           = "uxterm"
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
                     ,((mod4Mask, xK_l), unsafeSpawn "slimlock")
                     ,((0,xF86XK_MonBrightnessUp), spawn "light -A 10")
                     ,((0,xF86XK_MonBrightnessDown), spawn "light -U 10")
                     ,((0,xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume 0 +1.5%")
                     ,((0,xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume 0 -- -1.5%")
                     ,((0,xF86XK_AudioMute), spawn "pactl set-sink-mute 0 toggle")
                     ]
