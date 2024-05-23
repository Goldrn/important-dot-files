import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.ManageDocks

import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Ungrab
import XMonad.Util.SpawnOnce

import XMonad.Layout.Magnifier
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spacing

import XMonad.Hooks.EwmhDesktops

xmobar1 = statusBarProp "xmobar -x 0" (pure myXmobarPP)
xmobar2 = statusBarProp "xmobar -x 1" (pure myXmobarPP)
xmobar3 = statusBarProp "xmobar -x 2" (pure myXmobarPP)


--xmobar2 = statusBarPropTo "XMonadLog" "xmobar -x 1 ~/.xmobarrc" (pure myXmobarPP)
--xmobar3 = statusBarPropTo "XMonadLog" "xmobar -x 2 ~/.xmobarrc" (pure myXmobarPP)

main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (xmobar1 <> xmobar2 <> xmobar3) defToggleStrutsKey
     $ myConfig

myConfig = def
    { modMask    = mod4Mask      -- Rebind Mod to the Super key
    , layoutHook = myLayout      -- Use custom layouts
    , manageHook = myManageHook  -- Match on certain windows
    , startupHook = myStartupHook
    , normalBorderColor = "#83a598"
    , focusedBorderColor = "#d3869b"
    , borderWidth = 2
    }
  `additionalKeysP`
    [ ("M-<Return>", spawn "kitty")
    , ("M-;", spawn "rofi -show run")
    , ("M-t", spawn "nemo")
    , ("M-c", kill)
    , ("M-f", spawn "firefox")
    , ("M-z", sendMessage ToggleStruts)
    , ("M-d", spawn "discord")
    ]

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp" --> doFloat
    , isDialog            --> doFloat
    , className =? "RuneLite.AppImage" --> doFloat
    ]
myStartupHook :: X ()
myStartupHook = do
    spawnOnce "~/.fehbg &"
    spawnOnce "picom"
    spawnOnce "lxappearance"
    spawnOnce "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1"
    spawnOnce "udiskie &"

myLayout = smartSpacing 1 $ avoidStrutsOn [U] $ tiled ||| Full
  where
    tiled    = Tall nmaster delta ratio
    nmaster  = 1      -- Default number of windows in the master pane
    ratio    = 1/2    -- Default proportion of screen occupied by master pane
    delta    = 3/100  -- Percent of screen to increment by when resizing panes

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#FE8DBF" ""
    blue     = xmobarColor "#5BCEFA" ""
    white    = xmobarColor "#ebdbb2" ""
    yellow   = xmobarColor "#fabd2f" ""
    red      = xmobarColor "#cc241d" ""
    lowWhite = xmobarColor "#FFFFFF" ""
