{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Themes ( myTheme ) where

import XMonad.Layout.Decoration

import Config

activeBg   = base0D
activeFg   = base07
inactiveBg = base03
inactiveFg = base05
urgentBg   = base08
urgentFg   = base00

myTheme :: Theme
myTheme = defaultTheme { decoHeight = 18
                       , activeColor = activeBg
                       , activeBorderColor = activeBg
                       , activeTextColor = activeFg
                       , inactiveColor = inactiveBg
                       , inactiveBorderColor = inactiveBg
                       , inactiveTextColor = inactiveFg
                       , urgentColor = urgentBg
                       , urgentBorderColor = urgentBg
                       , urgentTextColor = urgentFg
                       , fontName = "xft:Source Code Pro:size=7:antialias=true" }
