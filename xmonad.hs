{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleContexts #-}

import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.FixedColumn
import           XMonad.Layout.Renamed
import           XMonad.Layout.Spacing

myWorkspaces = map show ([1..9] :: [Integer])

myLayout = avoidStruts $ spaced tiled ||| renamed [Replace "100Col"] emacs
  where
    tiled = Tall nmaster delta ratio
    emacs = FixedColumn nmaster 20 100 10
            -- FIXME: FixedColumn doesn't work with Layout.Spacing module
    spaced = smartSpacing 4
    nmaster = 1
    ratio = 1/2
    delta = 3/100

myManageHook :: ManageHook
myManageHook = composeAll [ className =? "mpv" --> doFloat ]

myXmobarPP = defaultPP { ppCurrent = xmobarColor "#859900" "" . wrap "[" "]"
                       }

toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b)

myXmobar = statusBar "xmobar" myXmobarPP toggleStrutsKey
myConfig = defaultConfig { terminal = "termite"
                         , workspaces = myWorkspaces
                         , layoutHook = myLayout
                         , manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
                         , modMask = mod4Mask
                         , borderWidth = 0
                         , handleEventHook = fullscreenEventHook
                         }

main :: IO ()
main = xmonad =<< myXmobar myConfig
