-- -*-haskell-*-
-- xmonad config for dzen

import XMonad
import XMonad.Core

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Man

import XMonad.Layout
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Accordion
import XMonad.Layout.Grid
import XMonad.Layout.Roledex


import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.SetWMName

import XMonad.Actions.CycleWS
import XMonad.Actions.FloatKeys
import XMonad.Actions.WindowGo
import XMonad.Actions.GroupNavigation

import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.Scratchpad
import Graphics.X11.Xlib
import qualified Data.Map as M
import System.IO

main = do
     xmproc <- spawnPipe "/usr/bin/xmobar /home/duran/.xmonad/xmobarrc"
     xmsess <- spawn "/home/duran/.xsession"
     xmonad $ myUrgencyHook $ defaultConfig
     	    { terminal = myTerminal
	    , focusFollowsMouse = False
     	    , normalBorderColor = myInactiveBorderColor
	    , focusedBorderColor = myActiveBorderColor
     	    , manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
     	    , layoutHook = smartBorders (avoidStruts $ myLayoutHook)
     	    , startupHook = setWMName "LG3D"
     	    , logHook = myLogHook xmproc
      	    , modMask = mod4Mask
     	    , keys = myKeys
     	    , workspaces = myWorkSpaces
     	    } `additionalKeysP` audioKeys

-- paths.
myBitmapsPath = ".dzen/bitmaps/"

myFont = "-*-fixed-*-*-*-*-*-*-*-*-*-*-*-*"
myTerminal = "urxvt"

-- Colors
myBgBgColor = "black"
myFgColor   = "gray80"
myBgColor   = "gray20"
myHighlightedFgColor = "white"
myHighlightedBgColor = "gray40"

myActiveBorderColor    = "gray80"
myInactiveBorderColor  = "gray20"

myCurrentWsFgColor     = "white"
myCurrentWsBgColor     = "gray40"
myVisibleWsFgColor     = "gray80"
myVisibleWsBgColor     = "gray20"
myHiddenWsFgColor      = "gray80"
myHiddenEmptyWsFgColor = "gray50"
myUrgentWsBgColor      = "brown"
myTitleFgColor	       = "white"

myUrgencyHintFgColor   = "white"
myUrgencyHintBgColor   = "brown"


-- dzen options
myDzenGenOpts	= "-fg '" ++ myFgColor ++ "' -bg '"
		++ myBgColor ++ "' -fn '" ++ myFont 
		++ "' -h '16'"

-- status bar
myStatusBar = "dzen2 -w 700 -ta l " ++ myDzenGenOpts

-- conky bar
myConkyBar = "conky -c ~/.conkybar | dzen2 -x 700 -w 666 -ta r " ++ myDzenGenOpts

myLayoutHook = smartBorders $ (Full ||| tiled ||| Mirror tiled ||| Roledex  ||| Accordion ||| Grid )
    where
	tiled = ResizableTall nmaster delta ratio []
	nmaster = 1
	delta = 3/100
	ratio = 1/2

myLogHook h = dynamicLogWithPP $ xmobarPP
            { ppOutput = hPutStrLn h
            , ppTitle = xmobarColor "green" "" . shorten 50
            , ppHidden = xmobarColor "white" "" . noScratchpad
            }
            where
                noScratchpad ws = if ws == "NSP" then "" else ws

-- workspaces
myWorkSpaces =
   [ "sys"
   , "web"
   , "txt"
   , "msg"
   , "avi"	     
   , "rdp"
   , "tmp"
   ]
--   where
--      wrapBitmap bitmap = "^p(5)^i(" ++ myBitmapsPath ++ bitmap ++ ")^p(5)"

myUrgencyHook = withUrgencyHook dzenUrgencyHook
	      {
		args = 
		     [ "-x", "0", "-y", "576", "-h", "15", "-w", "1024"
		     , "-ta", "r"
		     , "-fg", "" ++ myUrgencyHintFgColor ++ ""
		     , "-bg", "" ++ myUrgencyHintBgColor ++ ""
		     ]
	      }

myManageHook = composeAll [matchAny v --> a | (v, a) <- myActions] <+> manageScratchpad
    where myActions = [ ("rdesktop",  doShift "msg")
                      , ("Chromium",  doShift "web")
                      , ("Vlc",       doShift "avi")
                      , ("Emacs",     doShift "txt")
                      ]

-- pbrisbin's matchAny (http://pbrisbin.com/static/docs/haskell/xmonad-config/src/Utils.html)
matchAny :: String -> Query Bool
matchAny x = foldr ((<||>) . (=? x)) (return False) [className, title, name, role]

name :: Query String
name = stringProperty "WM_NAME"

role :: Query String
role = stringProperty "WM_ROLE"             

manageScratchpad :: ManageHook
manageScratchpad = scratchpadManageHook (W.RationalRect l t w h)
                 where
                        h = 0.5    -- height
                        w = 0.5    -- width
                        t = 0.25   -- distance from top
                        l = 0.25   -- distance from left
	  
myMatchAnywhereFloatsC = []
myMatchAnywhereFloatsT = []
myFloats	       = [ "Empathy", "Xmessage", "VirtualBox"]   

-- prompt
myXPConfig = defaultXPConfig {
	     position = Bottom,
	     promptBorderWidth = 0,
	     height = 15,
	     bgColor = myBgColor,
	     fgColor = myFgColor,
	     fgHLight = myHighlightedFgColor,
	     bgHLight = myHighlightedBgColor
	     }

-- Key bindings. Union the defaults with my keys.
myKeys x = M.union (M.fromList (newKeys x)) (keys defaultConfig x)

newKeys conf@(XConfig { XMonad.modMask = modm}) = 
	[ ((modm, xK_q), spawn "xmonad --recompile; killall trayer redshift xmobar; xmonad --restart")
	, ((modm .|. shiftMask, xK_b), runOrRaise "chromium" (className =? "Chromium"))
	, ((modm, xK_e), raiseEditor)
	, ((modm, xK_f), nextMatchWithThis Forward className)
	, ((modm, xK_b), nextMatchWithThis Backward className)
	, ((modm, xK_c), spawn "~/bin/chromeproxy")
	, ((modm, xK_grave), toggleWS)
	-- workspace cycling
	, ((modm, xK_Right), nextWS)
	, ((modm, xK_Left), prevWS)
	, ((modm .|. shiftMask, xK_Left), shiftToPrev >> prevWS)
	, ((modm .|. shiftMask, xK_Right), shiftToNext >> nextWS)
	, ((modm, xK_Up), moveTo Next EmptyWS)
        , ((modm, xK_s), scratchpadSpawnActionTerminal myTerminal)
	]

audioKeys :: [(String, X())]
audioKeys = [ ("<XF86AudioMute>"	, spawn "amixer -q set Master toggle" )
	    , ("<XF86AudioRaiseVolume>" , spawn "amixer -q set Master 10%+" )
	    , ("<XF86AudioLowerVolume>" , spawn "amixer -q set Master 10%-" )
	    ]

-- dzen config.
myDzenPP h = defaultPP 
	 { ppOutput	     = hPutStrLn h
	 , ppSep 	     = "^bg(" ++ myBgColor ++ ")^r(1,15)^bg()"
	 , ppWsSep  	     = ""
	 , ppCurrent	     = wrapFgBg myCurrentWsFgColor myCurrentWsBgColor
	 , ppVisible 	     = wrapFgBg myVisibleWsFgColor myVisibleWsBgColor
	 , ppHidden	     = wrapFg myHiddenWsFgColor
	 , ppHiddenNoWindows = wrapFg myHiddenEmptyWsFgColor
	 , ppUrgent	     = wrapBg myUrgentWsBgColor
	 , ppTitle	     = (\x -> " " ++ wrapFg myTitleFgColor x)
	 , ppLayout	     = dzenColor myFgColor"" .
	   		       (\x -> case x of
			       	   "ResizableTall" -> wrapBitmap "xbm/layout-tall.xbm"
				   "Mirror ResizableTall" -> wrapBitmap "xbm/layout-mtall.xbm"
				   "Full" -> wrapBitmap "xbm/layout-full.xbm"
				   "Grid" -> wrapBitmap "xbm/layout-grid.xbm"
				   "Accordion" -> wrapBitmap "xbm/layout-accordion.xbm"
			       )
	}
	where
		wrapFgBg fg bg content= wrap ("^fg(" ++ fg ++ ")^bg(" ++ bg ++ ")") "^fg()^bg()" content
		wrapFg color content = wrap ("^fg(" ++ color ++ ")") "^fg()" content
		wrapBg color content = wrap ("^bg(" ++ color ++ ")") "^bg()" content
		wrapBitmap bitmap = "^p(5)^i(" ++ myBitmapsPath ++ bitmap ++ ")^p(5)"
	 
		     
	