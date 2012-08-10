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
import XMonad.Actions.WindowBringer
import XMonad.Actions.GroupNavigation
import qualified XMonad.Actions.Submap as SM
import XMonad.Actions.Search


import XMonad.Prompt
import XMonad.Prompt.MPD
import XMonad.Prompt.Shell

import qualified Network.MPD as MPD
import qualified Network.MPD.Commands.Extensions as MPDE


import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig             (additionalKeysP)
import XMonad.Util.Run                  (spawnPipe)
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare     (getSortByIndex)
import XMonad.Util.XSelection
import XMonad.Util.Paste
import qualified Data.Map as M
import System.IO                        (hPutStrLn)
import Data.Maybe                       (isJust)
import Data.Either.Utils

main = do
     xmproc <- spawnPipe "/home/duran/.cabal/bin/xmobar /home/duran/.xmonad/xmobarrc"
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

-- Prompt
myPrompt = defaultXPConfig
           { font = myFont
           , bgColor = myBgColor 
           , fgColor = myFgColor
           , promptBorderWidth = 1
           , position = Top
           , height = 22
           , defaultText = []
           }
                           


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

myLayoutHook = smartBorders $ (Full ||| tiled ||| Roledex  ||| Accordion ||| Grid )
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
   , "mpd"  
   ]

myManageHook = composeAll 
             [matchAny v --> a | (v, a) <- myActions ] <+> manageScratchpad <+> myFullHook
             where myActions = [ ("rdesktop",  doShift "rdp")
                               , ("Chromium",  doShift "web")
                               , ("Vlc",       doShift "avi")
                               , ("Emacs",     doShift "txt")
                               , ("Wfica",     doShift "tmp")
                               , ("Wfica",     doFullFloat  )
                               , ("Iceweasel",   doShift "web")
                               ]

myFullHook = composeAll
           [ isFullscreen --> doFullFloat ]

-- pbrisbin's matchAny (http://pbrisbin.com/static/docs/haskell/xmonad-config/src/Utils.html)
matchAny :: String -> XMonad.Query Bool
matchAny x = foldr ((<||>) . (=? x)) (return False) [className, title, wmName, role]

wmName :: XMonad.Query String
wmName = stringProperty "WM_NAME"

role :: XMonad.Query String
role = stringProperty "WM_ROLE"             

manageScratchpad :: ManageHook
manageScratchpad = scratchpadManageHook (W.RationalRect l t w h)
                 where
                        h = 0.5    -- height
                        w = 0.5    -- width
                        t = 0.25   -- distance from top
                        l = 0.25   -- distance from left
                        
myScratchpads = [                       
  NS "music" "urxvt -name ncmpcpp -e ncmpcpp" (appName =? "ncmpcpp") (customFloating $ W.RationalRect 0.5 0.5 0.25 0.25)
  ]
                
mySearchEngine = intelligent (multi)
	  
myMatchAnywhereFloatsC = []
myMatchAnywhereFloatsT = []
myFloats	       = [ "Empathy", "Xmessage", "VirtualBox"]   

-- Key bindings. Union the defaults with my keys.
myKeys x = M.union (M.fromList (newKeys x)) (keys defaultConfig x)

newKeys conf@(XConfig { XMonad.modMask = modm}) = 
	[ ((modm, xK_q), spawn "xmonad --recompile; killall redshift xmobar; xmonad --restart")
	, ((modm .|. shiftMask, xK_b), runOrRaise "chromeproxy" (className =? "Chromium"))
        , ((modm, xK_c), runOrRaise "chromium" (className =? "Chromium"))  
        , ((modm, xK_m), namedScratchpadAction myScratchpads "music")  
        , ((modm, xK_u), safePromptSelection "chromium")
	, ((modm, xK_e), runOrRaise "emacs" (className =? "Emacs"))
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
        , ((modm .|. shiftMask, xK_s), shellPrompt myPrompt)
        , ((modm, xK_y), pasteSelection)
        , ((modm, xK_a), addAndPlay MPD.withMPD myPrompt [MPD.Artist,MPD.Album] >> return ())
        , ((modm .|. shiftMask, xK_a), addAndPlay MPD.withMPD myPrompt [MPD.Album] >> return ())
        , ((modm, xK_z), addAndPlay MPD.withMPD myPrompt [MPD.Title] >> return ())
        , ((modm, xK_slash), SM.submap $ searchEngineMap $ promptSearchBrowser  myPrompt "chromium")
        , ((modm .|. shiftMask, xK_slash), SM.submap $ searchEngineMap $ selectSearchBrowser "chromium")
        , ((modm, xK_g), gotoMenu)
        , ((modm .|. shiftMask, xK_g), bringMenu)  
        ]
        where skipEmptyAndSP = (WSIs $ noEmptyOrSP ["NSP"])

searchEngineMap method = M.fromList $
                         [ ((0, xK_slash), method google)
                         , ((0, xK_w), method wikipedia)
                         , ((0, xK_h), method hoogle)
                         , ((0, xK_i), method images)
                         , ((0, xK_y), method youtube)
                         , ((0, xK_m), method maps)  
                         , ((0, xK_a), method alpha)  
                         , ((0, xK_l), method cliki)  
                         , ((0, xK_p), method perldoc)  
                         ]  
                         where 
                           cliki     = searchEngine "cliki" "http://www.cliki.net/admin/search?words="
                           perldoc   = searchEngine "perldoc" "http://perldoc.perl.org/search.html?q="

  


noSPWS dir wtype = do t <- findWorkspace getSortByIndex dir wtype 1
                      windows . W.greedyView $ t

noEmptyOrSP s = return (\w -> (W.tag w `notElem` s) && isJust (W.stack w))

audioKeys :: [(String, X())]
audioKeys = [ ("<XF86AudioMute>"	, spawn "amixer -q set Master toggle" )
	    , ("<XF86AudioRaiseVolume>" , spawn "amixer -q set Master 10%+" )
	    , ("<XF86AudioLowerVolume>" , spawn "amixer -q set Master 10%-" )
            , ("<XF86Forward>"          , moveTo Next NonEmptyWS)
            , ("<XF86Back>"             , moveTo Prev NonEmptyWS)
              -- MPD keys --
            , ("<XF86AudioPlay>", io $ return . fromRight =<< MPD.withMPD MPDE.toggle )
            , ("<XF86AudioStop>", io $ return . fromRight =<< MPD.withMPD MPD.clear)  
            , ("<XF86AudioNext>", io $ return . fromRight =<< MPD.withMPD MPD.next)
            , ("<XF86AudioPrev>", io $ return . fromRight =<< MPD.withMPD MPD.previous)              
              -- I'm being lazy here and just adding these under audioKeys... --
            , ("<XF86Sleep>"            , spawn "sudo pm-suspend")
            , ("<XF86Display>"          , spawn "/home/duran/bin/screen-toggle")
	    ]

		     
toggleOrViewNoSP = toggleOrDoSkip ["NSP"] W.greedyView	


-- MPD stuff follows along with braying and neighing of barnyard animals.

--addMatching MPD.withMPD defaultXPConfig [MPD.Artist, MPD.Album] >> return ()



