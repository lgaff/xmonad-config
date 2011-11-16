-- Requires pbrisbin's dzen module loaded into .xmonad/lib
-- (http://pbrisbin.com/xmonad/docs/src/Dzen.html)


import System.IO (hPutStrLn)
import XMonad
import Dzen
import XMonad.Hooks.DynamicLog hiding (dzen)
import XMonad.Hooks.UrgencyHook
import XMonad.Util.EZConfig
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Actions.FloatKeys
import Data.List

main :: IO ()
-- main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig
main = do
   d <- spawnDzen leftSide
   spawnToDzen "conky" rightSide
   xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
        { terminal            = "urxvt"
        , workspaces          = ["1:term","2:web","3:vlc","4:work","5:float","6:msg","7","8","9"]
        , modMask             = mod4Mask
        , focusFollowsMouse   = False
        , startupHook         = setWMName "LG3D"
        , manageHook          = myManageHook <+> manageHook defaultConfig <+> manageDocks
        , layoutHook          = avoidStruts $ layoutHook defaultConfig
        , logHook             = myLogHook d
        } `additionalKeysP` myKeys


myKeys :: [(String, X())]
myKeys =    [ ("<XF86AudioMute>"          , spawn "amixer -q set Master toggle" )
            , ("<XF86AudioRaiseVolume>"   , spawn "amixer -q set Master 10%+" )
            , ("<XF86AudioLowerVolume>"   , spawn "amixer -q set Master 10%-" )
            , ("M-b"                      , spawn "chromium" )
            , ("M-S-b"                    , spawn "chrome-proxy" )
            , ("M-q"                      , cleanStart )
            ]

-- workspace hooks.
myManageHook = composeAll . concat $
               [ [ className =? "URxvt"      --> doShift "1:term" ]
               , [ className =? "Chromium"   --> doShift "2:web" ]
               , [ className =? "Vlc"        --> doShift "3:vlc" ]
               , [ className =? "Empathy"    --> doShift "6:msg" ]
               , [ className =? c --> doFloat | c <- myFloats ]                               -- float by explicit list
               , [ fmap (c `isInfixOf`) className --> doFloat | c <- myMatchAnywhereFloatsC ] -- float by classname regex-ish
               , [ fmap (c `isInfixOf`) className --> doFloat | c <- myMatchAnywhereFloatsT ] -- float by title regex-ish
               ] 

myMatchAnywhereFloatsC  = []
myMatchAnywhereFloatsT  = []
myFloats                = ["Gvim", "Empathy", "Xmessage"]

-- Dzen log hook stuff (lifted from pbrisbin's tutorial.)
myLogHook h = dynamicLogWithPP $ defaultPP
   { ppCurrent          = dzenColor "#303030" "#909090" . pad
   , ppHidden           = dzenColor "#909090" "" . pad
   , ppHiddenNoWindows  = dzenColor "#606060" "" . pad
   , ppLayout           = dzenColor "#909090" "" . pad
   , ppUrgent           = dzenColor "#FF0000" "" . pad . dzenStrip
   , ppTitle            = shorten 100
   , ppWsSep            = ""
   , ppSep              = "  "
   , ppOutput           = hPutStrLn h -- Make it so number 1.
   }


-- dzen status bars (again, straight from pbrisbin)
-- I may add a third here later, it would be nice to have an
-- xmonad logo at top left.

leftSide :: DzenConf
leftSide = defaultDzen
   { width     = Just $ Percent 60
   , fgColor   = Just "#909090"
   , bgColor   = Just "#303030"
   }

rightSide :: DzenConf
rightSide = leftSide
   { xPosition = Just $ Percent 60
   , width     = Just $ Percent 40
   , alignment = Just RightAlign
   }

-- clean start code courtesy (sort of) pbrisbin. Fixes the leftover dzen/conky
-- sessions after a mod-q restart
--
cleanStart :: MonadIO m => m ()
cleanStart = spawn $ "pgrep conky | xargs kill -9 &&"
                  ++ "pgrep dzen2 | xargs kill -9 &&"
                  ++ "xmonad --recompile && xmonad --restart"

