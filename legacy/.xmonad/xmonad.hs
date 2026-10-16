import Data.List (find)
import Graphics.X11.ExtraTypes.XF86
import XMonad
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks (ToggleStruts (..), avoidStruts, docks, manageDocks)
import XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen, (^?))
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.TaffybarPagerHints (pagerHints)
import XMonad.Layout.NoBorders (smartBorders)
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (unsafeSpawn)

main :: IO ()
main = do
    spawn "xsetroot -cursor_name left_ptr"
    spawn "xsetroot -solid darkgray"
    spawn "xsettingsd"
    spawn "xscreensaver -nosplash"
    spawn "emacs"
    spawn "xset -b" -- kill the system bell
    spawn "nm-applet --indicator"
    spawn "pasystray"
    spawn "parcellite -n"
    spawn "blueberry-tray"
    -- dunno what broke # redshift -O 6500 # doesnt even reset
    -- spawn "redshift -l -33.84:151.21 -t 6500:3500"
    xmonad $
        docks $
            pagerHints $
                ewmh $
                    ewmhFullscreen
                        def
                            { modMask = mod4Mask
                            , terminal = "st -f \"DejaVu Sans Mono:pixelsize=60:style=Book\" tmux"
                            , focusedBorderColor = "#cc6666"
                            , normalBorderColor = "#373b41"
                            , borderWidth = 3
                            , startupHook = setWMName "LG3D"
                            , manageHook =
                                (title ^? "NormCap [" --> doFloatOnOwnScreen)
                                    <+> manageDocks
                                    <+> (isFullscreen --> doFullFloat)
                                    <+> manageHook def
                                    <+> (title =? "ediff" --> doFloat)
                            , layoutHook = smartBorders $ avoidStruts $ layoutHook def
                            }
                        `additionalKeys` [ ((mod4Mask, xK_b), sendMessage ToggleStruts)
                                         , ((mod4Mask, xK_p), spawn "PATH=$PATH:~/bin dmenu_run -fn \"DejaVu Sans Mono:pixelsize=20:style=Book\"")
                                         , ((mod4Mask .|. shiftMask, xK_l), unsafeSpawn "xscreensaver-command -lock")
                                         , ((0, xF86XK_MonBrightnessUp), spawn "sudo xbacklight -inc 10")
                                         , ((0, xF86XK_MonBrightnessDown), spawn "sudo xbacklight -dec 10")
                                         , -- ,((0,xF86XK_KbdBrightnessUp), spawn "kbdlight up")
                                           -- ,((0,xF86XK_KbdBrightnessDown), spawn "kbdlight down")
                                           ((0, xF86XK_AudioRaiseVolume), spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+")
                                         , ((0, xF86XK_AudioLowerVolume), spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-")
                                         , ((0, xF86XK_AudioMute), spawn "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle")
                                         , ((mod4Mask, xK_c), spawn "find-cursor -c red")
                                         , ((mod4Mask, xK_n), spawn "networkmanager_dmenu")
                                         , ((mod4Mask, xK_u), spawn "sendkeys alias_")
                                         , ((mod4Mask, xK_o), spawn "sendkeys password")
                                         , ((mod4Mask, xK_i), spawn "sendkeys totp")
                                         , ((mod4Mask .|. shiftMask, xK_t), spawn "st -f \"DejaVu Sans Mono:pixelsize=20:style=Book\"")
                                         , ((mod4Mask .|. shiftMask, xK_n), spawn "normcap")
                                         ]

-- float a window on the screen it placed itself on (normcap opens one
-- fullscreen overlay per screen; doFullFloat would stack them all on the
-- focused screen)
doFloatOnOwnScreen :: ManageHook
doFloatOnOwnScreen = do
    w <- ask
    (sid, r) <- liftX (floatLocation w)
    doF $ \ws -> case find ((== sid) . W.screen) (W.screens ws) of
        Just s -> W.float w r (W.shiftWin (W.tag (W.workspace s)) w ws)
        Nothing -> W.float w r ws
