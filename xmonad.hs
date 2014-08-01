{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.Decoration
import           XMonad.Layout.FixedColumn
import           XMonad.Layout.Renamed

import           Config
import           Padding
import           Themes

import qualified Data.Map as M

myWorkspaces = map show ([1..9] :: [Integer])

myLayout = avoidStruts $ decoTiled' ||| decoTiled ||| emacs ||| decoCentered
  where
    tiled    = spaced $ Tall nmaster delta ratio
    tiled'   = spaced' $ Tall nmaster delta ratio
    emacs    = renamed [Replace "100Col"] $ FixedColumn nmaster 20 100 10
               -- FIXME: FixedColumn doesn't work with Layout.Spacing module
    spaced   = padding 5 5
    spaced'  = padding 40 40
    centered = padding 0 420 Full
    nmaster = 1
    ratio = 1/2
    delta = 3/100

    decoTiled  = decorateLayout tiled myTheme
    decoTiled' = decorateLayout tiled' myTheme
    decoCentered = decorateLayout centered myTheme

    decorateLayout l t = decoration shrinkText t DefaultDecoration l

myManageHook :: ManageHook
myManageHook = composeAll [ className =? "mpv" --> doFloat ]

myXmobarPP = defaultPP { ppCurrent = xmobarColor "#859900" "" . wrap "[" "]" }

toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b)

myKeysList modm = [ ((modm, xK_p), spawn "j4-dmenu-desktop") ]

myKeys' (XConfig { XMonad.modMask = modm }) = M.fromList $ myKeysList modm

myKeys x = myKeys' x `M.union` keys defaultConfig x

myXmobar = statusBar "xmobar" myXmobarPP toggleStrutsKey
myConfig = defaultConfig { terminal = "termite"
                         , workspaces = myWorkspaces
                         , layoutHook = myLayout
                         , manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
                         , modMask = mod4Mask
                         , borderWidth = 0
                         , handleEventHook = fullscreenEventHook
                         , keys = myKeys }

main :: IO ()
main = xmonad =<< myXmobar myConfig
