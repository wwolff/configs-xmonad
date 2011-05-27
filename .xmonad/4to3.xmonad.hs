{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-} -- for TallAlt
--
-- sereven xmonad.hs,  0.9.2 - 0.10,  v0.10.4 2011-05-26
--
-- TODO
--      add notification daemon of xwininfo -shape on focused
--      Maybe with conversion to Rational Rect x,y,w,h
--      and xgeometry too. (See also Hooks.Place.)
--
--      old GNOME one (notify-send from libnotify):
--      echo "some\ info\ here" | xargs \
--      notify-send -u critical -t 8000 -i \
--      ~/.icons/Glossy-Glass/128x128/apps/gnome-logout.png -h \
--      int:"x":12 -h int:"y":1100 ' this is the title header'
--      -u can be low || normal || critical
--      Can use html formatting.

-- imports {{{

import XMonad hiding (keys)
import qualified XMonad.StackSet as W

-- standard libraries
import Control.Applicative ((<$>))
import Control.Monad (liftM2)
import Data.List (isPrefixOf, nub)
import Data.Maybe (fromMaybe)
import System.Exit

-- xmonad-contrib (darcs || 0.10)
import XMonad.Actions.CycleWS (swapNextScreen, toggleOrDoSkip, nextWS, prevWS)
import XMonad.Actions.FlexibleManipulate as Flex
import XMonad.Actions.OnScreen (onlyOnScreen)
import XMonad.Actions.Search hiding (isPrefixOf)
import XMonad.Actions.WindowNavigation
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.IM
import XMonad.Layout.LayoutHints
import XMonad.Layout.LimitWindows
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.WorkspaceDir
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise
import XMonad.Util.EZConfig
import XMonad.Util.Run (hPutStrLn, spawnPipe)
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare
-- }}}

wsIds = map return "123456789" ++ ["NSP"]

rr = W.RationalRect -- fractions of screen: x y w h

infixr 0 ~> -- <http://mauke.ath.cx/stuff/xmonad/xmonad.hs>
(~>) :: a -> b -> (a, b)
(~>) = (,)

-- main {{{
main = do
    dz <- spawnPipe themedDzen
    conf <- withNavKeys (xK_k, xK_h, xK_j, xK_l) $
        gnomeConfig
            { terminal           = "urxvt"
            , modMask            = mod4Mask
            , normalBorderColor  = bgColor promptConfig
            , focusedBorderColor = fgColor promptConfig
            , workspaces         = wsIds
            , logHook            = dynamicLogWithPP dzPP { ppOutput = hPutStrLn dz }
            , manageHook         = manageHooks
            , layoutHook         = layouts
            }
        `additionalKeysP` keys `additionalMouseBindings` buttons
    xmonad conf { startupHook = do
                    startupHook gnomeConfig
                    checkKeymap conf keys
                    setWMName "LG3D"
                    windows $ onlyOnScreen 1 "8"
                    }
-- }}}

-- scratchpads {{{
pads =
    [ NS "nauti" "nautilus --browser --sm-client-disable" (resource =? "nautilus") nautiHook
    , NS "scratch" "urxvt -pe tabbed -name scratch" (resource =? "scratch") scratchHook
    , NS "tote" "cryptote" (resource =? "cryptote") toteHook
    ]
  where
    scratchHook =
      doF W.shiftMaster <+> (doRectFloat $ rr 0.272 0.461 0.58374 0.32)
    nautiHook =
      doF W.shiftMaster <+> (doRectFloat $ rr 0.45 0.19 0.5 0.65)
    toteHook =
      doF W.shiftMaster <+> (doRectFloat $ rr 0.1 0.56 0.6 0.38)
-- }}}

-- keyboard and mouse {{{

-- uses mod4 on logo key and caps lock
-- via `Option "XkbOptions" "caps:super"'

withNavKeys (u,l,d,r) = withWindowNavigationKeys
    [ (mod4Mask             , u) ~> WNGo   U
    , (mod4Mask             , l) ~> WNGo   L
    , (mod4Mask             , d) ~> WNGo   D
    , (mod4Mask             , r) ~> WNGo   R
    , (mod4Mask .|. mod1Mask, u) ~> WNSwap U
    , (mod4Mask .|. mod1Mask, l) ~> WNSwap L
    , (mod4Mask .|. mod1Mask, d) ~> WNSwap D
    , (mod4Mask .|. mod1Mask, r) ~> WNSwap R ]

buttons =
    [ (mod4Mask             , button3) ~> Flex.mouseWindow Flex.discrete
    , (mod4Mask             , button4) ~> const $ windows W.swapDown
    , (mod4Mask             , button5) ~> const $ windows W.swapUp
    , (mod1Mask .|. mod4Mask, button4) ~> const nextWS
    , (mod1Mask .|. mod4Mask, button5) ~> const prevWS ]

keys = -- 
    [ "M-C-S-q"       ~> io (exitWith ExitSuccess)
--  , "M-S-q"         ~> spawn "xmessage -- [ Ctrl+Shift+Mod4+Q to exit; no gnome-session mgr] --"
    , "M-<Space>"     ~> sendMessage ToggleLayout  -- toggle fullscreen
    , "M-c"           ~> sendMessage NextLayout
    , "M-<Return>"    ~> windows $ W.focusMaster
    , "M-M1-<Return>" ~> windows $ W.focusUp . W.focusMaster
    , "M-M1-,"        ~> sendMessage Shrink
    , "M-M1-."        ~> sendMessage Expand
    ]
    -- workspaces and screens  -- 1 2 3 \
                               --  q w e \
    ++                         --   a s d \ f g
    [ "M-v" ~> swapNextScreen] --          \ v (b)
    ++
    [ mask ++ [key] ~> action i | (key, i) <- zip "123qweasd=" wsIds
         , (mask, action) <- [ ("M-", toggled W.greedyView)
                             , ("M-M1-", toggled followShift) ] ]
    ++
    [ mask ++ [key] ~> screenWorkspace s >>= flip whenJust (windows . action)
         | (key, s) <- zip "fg" [0..]
         , (mask, action) <- [ ("M-", W.view)
                             , ("M-M1-", W.shift) ] ]
    ++ -- toolbox
    [ "M-<Tab>"   ~> namedScratchpadAction pads  "scratch"
    , "M-<F1>"    ~> namedScratchpadAction pads  "nauti"
    , "M-<F2>"    ~> namedScratchpadAction pads  "tote"
--    , "M-<F12>"   ~> windows copyToAll
    , "M-r"       ~> runOrRaisePrompt promptConfig
    , "M-M1-r"    ~> changeDir promptConfig ]
    ++ --use intelligent search instead?
    [ "M-/ "    ++ ks ~> promptSearch promptConfig s | (ks,s) <- searches ]
    ++
    [ "M-M1-/ " ++ ks ~> selectSearch s | (ks,s) <- searches ]
  where
    searches = [ ("f"  , fgo)
               , ("r"  , rseek)
               , ("s"  , scroogle)
               , ("x"  , xm_gmane)
               , ("g"  , google)
               , ("i"  , images)
               , ("w"  , wikipedia)
               ]
    fgo      = searchEngineF "gentoo forums" $
                  wrap "http://www.google.com/search?q="
                       "+site%3Aforums.gentoo.org+-inurl%3Asearch.php" . escape
    rseek    = searchEngineF "RSeek" $
                  wrap "http://www.rseek.org/?cx=010923144343702598753%3Aboaz1reyxd4&newwindow=1&q="
                       "&sa=Search&cof=FORID%3A11&siteurl=rseek.org%252F#1666" . escape
    scroogle = searchEngine  "scroogle"
                  "https://ssl.scroogle.org/cgi-bin/nbbwssl.cgi?Gw="
    xm_gmane = searchEngine  "xmonad ml"
                  "http://search.gmane.org/?group=gmane.comp.lang.haskell.xmonad&query="

    toggled = toggleOrDoSkip ["NSP"]
    followShift = liftM2 (.) W.view W.shift
-- }}}

-- manage hook {{{
manageHooks = composeOne
    [ transience
    , isDialog                -?> doF W.shiftMaster <+> doFloat
    , ("libreoffice" `isPrefixOf`) <$> className -?> doShift "3"
    , ("Gimp"        `isPrefixOf`) <$> className -?> doShift "5"
    , title =? "ReminderFox"  -?> doF W.shiftMaster <+> doRectFloat (rr 0.15 0.465 0.75 0.365)
    , title =? "Quick Alarm"  -?> doF W.shiftMaster <+> doFloat
    , role =? "reminderFoxEdit" -?> doF W.shiftMaster <+> doCenterFloat
    , className =? "Firefox"  -?> doShift "8"
    , isMPlayerFull           -?> doShift "5"
    , className =? "Zathura"  -?> doShift "2"
    , className =? "XFontSel" -?> doF W.shiftMaster <+> doCenterFloat
    , className =? "Xmessage" -?> doF W.shiftMaster <+> doCenterFloat
    , className =? "feh"      -?> doF W.shiftMaster <+> doFloat
    , className =? "Qjackctl" -?> doFloat
    , className =? ""         -?> doFloat -- low budget gtk windows
    ] <+> namedScratchpadManageHook pads <+> manageDocks
  where
    role = stringProperty "WM_WINDOW_ROLE"
    isMPlayerFull = (className =? "MPlayer" <&&> appName =? "gl2")
                        <||> title =? "Gnome MPlayer Fullscreen"
-- }}}

-- layouts {{{

-- TallAlt from <http://www.haskell.org/pipermail/xmonad/2009-July/008270.html>
data TallAlt a = TallAlt
    { tallAltIncrement :: !Rational
    , tallAltRatio :: !Rational
    } deriving (Read, Show)

instance LayoutClass TallAlt a where
    doLayout (TallAlt i d) r st =
     fmap (\(x,_) -> (x,Nothing)) $ doLayout (Tall nmaster i d) r st
        where nmaster | stlen > 3 = 2
                      | otherwise = 1
              stlen = length $ W.integrate st
    pureMessage (TallAlt i d) m = (`fmap` fromMessage m) $ \x -> case x of
        Expand -> TallAlt i (d+i)
        Shrink -> TallAlt i (d-i)

layouts =
    modifiers . onWorkspace "1" (Mirror (Tall 1 i 0.765) ||| Tall 1 i 0.5)
              . onWorkspaces ["2", "3", "4"] (workspaceDir cwd many) . onWorkspace "5" gimp
              . onWorkspace "7" (workspaceDir "~/.xmonad" four) $ workspaceDir "~" many
  where
    cwd  = "~/doc"
    i = 0.00125
    modifiers =
        smartBorders . toggleLayouts (noBorders Full) . layoutHintsToCenter . avoidStruts
    four = limitWindows 4 . Mirror $ TallAlt i (31/44) ||| Full
    gimp = workspaceDir "~/images" . reflectHoriz $ withIM 0.145 (Role "gimp-toolbox") four
    many = Tall 1 i (13/22) ||| Full ||| Mirror (TallAlt i (31/44))
-- }}}

-- prompt and dzen {{{

promptConfig = defaultXPConfig
    { font = "xft:Consolas:12"
    , bgColor  = "gray5"
    , fgColor  = "wheat3"
    , fgHLight = "DodgerBlue3"
    , bgHLight = "black"
    , promptBorderWidth = 0
    , height   = 24
    , historyFilter = nub
    , showCompletionOnTab = True
    }

themedDzen = "dzen2 -xs 1 -x 25 -ta l -e 'onstart=lower'"
      ++ " -bg " ++ "'" ++ bgColor promptConfig ++ "'"
      ++ " -fg " ++ "'" ++ fgColor promptConfig ++ "'"
      ++ " -fn Denmark:Thin:size=11"
      ++ " -h "  ++ show (height promptConfig + 2)

dzPP  = defaultPP
    { ppCurrent         = \i -> wsColorCurrent i (wrap "|" "|" (wsIcon i)) ++ "^p(6;)"
    , ppVisible         = \i -> wsColorVisible i (wrap "|" "|" (wsIcon i)) ++ "^p(6;)"
    , ppHidden          = fg "wheat4" . wsIcon
    , ppHiddenNoWindows = fg "gray28" . wsIcon
    , ppWsSep           = "^p(6;)"
    , ppTitle           = take 108 . dzenEscape
    , ppSep             = ""
    , ppSort            = getSortByXineramaRule -- ensure wsIds alphabetical
    , ppOrder           = ppo
    }
  where
    wsIcon = wrap "^i(" ")" . ("/home/gvg/.config/dzen/icons/" ++) . wrap "ws-" ".xbm"
    ppo (ws:_:t:_) = ["^p(18;)", ws, "^p(30;).: ", t, " :."]
    ppo _          = ["Your logHook is broken."]

fg ::  String -> String -> String
fg c = dzenColor c ""

wsColorCurrent ::  String -> String -> String
wsColorCurrent i =
    fg . fromMaybe "wheat2" . lookup i . zip wsIds $
            concatMap (replicate 3) ["DarkOrchid1", "DodgerBlue1", "OliveDrab2"]

wsColorVisible ::  String -> String -> String
wsColorVisible i =
    fg . fromMaybe "wheat4" . lookup i . zip wsIds $
            concatMap (replicate 3) ["DarkOrchid3", "DodgerBlue4", "OliveDrab4"]
-- }}}

-- vim:foldmethod=marker
