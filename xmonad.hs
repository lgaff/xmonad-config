-- -*-haskell-*-
-- xmonad config for dzen

import XMonad

import XMonad.Layout.NoBorders          (smartBorders)
import XMonad.Layout.ResizableTile      
import XMonad.Layout.Accordion          
import XMonad.Layout.Grid               
import XMonad.Layout.Roledex            

import XMonad.Hooks.DynamicLog          
import XMonad.Hooks.ManageDocks         (manageDocks, avoidStruts)
import XMonad.Hooks.SetWMName           (setWMName)
import XMonad.Hooks.ManageHelpers

import XMonad.Actions.CycleWS
import XMonad.Actions.WindowGo 
import XMonad.Actions.GroupNavigation

import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig             (additionalKeysP)
import XMonad.Util.Run                  (spawnPipe)
import XMonad.Util.Scratchpad           (scratchpadManageHook, scratchpadSpawnActionTerminal)
import XMonad.Util.WorkspaceCompare     (getSortByIndex)
import qualified Data.Map as M
import System.IO                        (hPutStrLn)
import Data.Maybe                       (isJust)

main = do
     xmproc <- spawnPipe "/usr/bin/xmobar /home/duran/.xmonad/xmobarrc"
     xmonad $ defaultConfig
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

myFont = "xft:Anonymous Pro:size=10:regular"
--myFont = "-*-fixed-*-*-*-*-*-*-*-*-*-*-*-*"
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

myLayoutHook = smartBorders $ (Full ||| Mirror tiled ||| Roledex  ||| Accordion ||| Grid )
    where
	tiled = ResizableTall nmaster delta ratio []
	nmaster = 1
	delta = 3/100
	ratio = 1/2

myLogHook h = dynamicLogWithPP $ xmobarPP
            { ppOutput = hPutStrLn h
            , ppTitle = xmobarColor "#b58900" "" . shorten 50
            , ppHidden = xmobarColor "#93a1a1" "" . noScratchpad
            , ppCurrent = xmobarColor "#268bd2" ""
            , ppLayout = xmobarColor "#2aa198" ""
            , ppWsSep = "::"
            , ppSep = " : "
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

myManageHook = composeAll 
             [matchAny v --> a | (v, a) <- myActions ] <+> manageScratchpad <+> myFullHook
             where myActions = [ ("rdesktop",  doShift "msg")
                               , ("Chromium",  doShift "web")
                               , ("Vlc",       doShift "avi")
                               , ("Emacs",     doShift "txt")
                               , ("Wfica",     doShift "tmp")
                               , ("Wfica",     doFullFloat  )
                               , ("Firefox",   doShift "web")
                               ]

myFullHook = composeAll
           [ isFullscreen --> doFullFloat ]

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

-- Key bindings. Union the defaults with my keys.
myKeys x = M.union (M.fromList (newKeys x)) (keys defaultConfig x)

newKeys conf@(XConfig { XMonad.modMask = modm}) = 
	[ ((modm, xK_q), spawn "xmonad --recompile; killall redshift xmobar; xmonad --restart")
	, ((modm .|. shiftMask, xK_b), runOrRaise "firefox" (className =? "Firefox"))
	, ((modm, xK_e), raiseEditor)
	, ((modm, xK_f), nextMatchWithThis Forward className)
	, ((modm, xK_b), nextMatchWithThis Backward className)
	, ((modm, xK_grave), toggleWS' ["NSP"])
	-- workspace cycling
	, ((modm, xK_Right), moveTo Next skipEmptyAndSP)
	, ((modm, xK_Left),  moveTo Prev skipEmptyAndSP)
	, ((modm .|. shiftMask, xK_Right), shiftTo Next skipEmptyAndSP)
	, ((modm .|. shiftMask, xK_Left), shiftTo Prev skipEmptyAndSP)
	, ((modm, xK_Up), moveTo Next EmptyWS)
        , ((modm .|. shiftMask, xK_Up), shiftTo Next EmptyWS)
        , ((modm, xK_s), scratchpadSpawnActionTerminal myTerminal)
	]
        where skipEmptyAndSP = (WSIs $ noEmptyOrSP ["NSP"])

noSPWS dir wtype = do t <- findWorkspace getSortByIndex dir wtype 1
                      windows . W.greedyView $ t

noEmptyOrSP s = return (\w -> (W.tag w `notElem` s) && isJust (W.stack w))

audioKeys :: [(String, X())]
audioKeys = [ ("<XF86AudioMute>"	, spawn "amixer -q set Master toggle" )
	    , ("<XF86AudioRaiseVolume>" , spawn "amixer -q set Master 10%+" )
	    , ("<XF86AudioLowerVolume>" , spawn "amixer -q set Master 10%-" )
            , ("<XF86Forward>"          , moveTo Next NonEmptyWS)
            , ("<XF86Back>"             , moveTo Prev NonEmptyWS)
	    ]

		     
toggleOrViewNoSP = toggleOrDoSkip ["NSP"] W.greedyView	
