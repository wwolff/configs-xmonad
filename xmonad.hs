{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-} -- for TallAlt
--
-- sereven xmonad.hs,  0.9.2 - 0.10,  v0.10.8 2011-09-28
--

-- imports {{{

import XMonad hiding (keys)
import qualified XMonad.StackSet as W

-- standard libraries
import Control.Applicative ((<$>))
import Control.Monad (liftM2)
--import Data.List (isPrefixOf, isInfixOf, nub)
import Data.List (isPrefixOf, nub)
import Data.Maybe (fromMaybe)
import System.Exit

-- xmonad-contrib (darcs || 0.10)
import XMonad.Actions.CycleWS
import XMonad.Actions.FlexibleManipulate as Flex
import XMonad.Actions.OnScreen (onlyOnScreen)
import XMonad.Actions.Search hiding (isPrefixOf)
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowNavigation
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
--import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.IM
import XMonad.Layout.LayoutHints
import XMonad.Layout.LimitWindows
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.Spacing
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.WorkspaceDir
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise
import XMonad.Util.EZConfig
import XMonad.Util.Run (hPutStrLn, spawnPipe)
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare
-- }}}

-- Workspace ID list is used all over the place
wsIds = map return "123456789" ++ ["NSP"]

-- Infix (,) to clean up key and mouse bindings
infixr 0 ~>
(~>) :: a -> b -> (a, b)
(~>) = (,)
 
-- scratchpad descriptions - used in key bindings and manageHook {{{

pads =
    [ NS "asunder" "asunder" asunderQuery asunderHook
    , NS "nautilus" "nautilus --browser --sm-client-disable" (resource =? "nautilus") nautilusHook
    , NS "scratch" "urxvt -pe tabbed -name scratch" (resource =? "scratch") scratchHook
    , NS "tote" "cryptote" (resource =? "cryptote") toteHook
    ]
  where
    asunderQuery = ("Asunder" `isPrefixOf`) <$> title
    asunderHook  = doRectFloat $ rr 0.61 0.156 0.3 0.72
    scratchHook  = doRectFloat $ rr 0.38 0.07 0.508 0.3
    nautilusHook = doRectFloat $ rr 0.45 0.19 0.5 0.65
    toteHook = doRectFloat $ rr 0.1 0.36 0.6 0.58
    rr = W.RationalRect -- in fractions of screen: x y w h

-- }}}

-- main {{{

main = do
    dz <- spawnPipe themedDzen
    conf <- withNavKeys (xK_k, xK_h, xK_j, xK_l) $
        gnomeConfig
            { terminal           = "urxvt"
            , modMask            = mod4Mask
            , normalBorderColor  = fgColor promptConfig
            , focusedBorderColor = bgColor promptConfig
            , borderWidth        = 2
            , workspaces         = wsIds
            , manageHook         = namedScratchpadManageHook pads <+> manageHooks
            , layoutHook         = layouts
            }
        `additionalKeysP` keys `additionalMouseBindings` buttons
    xmonad conf
        { startupHook = do
            startupHook gnomeConfig
            checkKeymap conf keys
            setWMName "LG3D"
            windows $ onlyOnScreen 1 "8"
--          spawn "xcompmgr -Cc -r 3 -l -5 -t -5 &"
        , logHook = do
            updatePointer $ Relative 0.88 0.88
            dynamicLogWithPP dzPP { ppOutput = hPutStrLn dz }
--          fadeMostInactives 0.89 -- plus xcompmgr
        }
      where
        withNavKeys (u,l,d,r) = withWindowNavigationKeys
            [ (mod4Mask             , u) ~> WNGo   U
            , (mod4Mask             , l) ~> WNGo   L
            , (mod4Mask             , d) ~> WNGo   D
            , (mod4Mask             , r) ~> WNGo   R
            , (mod4Mask .|. mod1Mask, u) ~> WNSwap U
            , (mod4Mask .|. mod1Mask, l) ~> WNSwap L
            , (mod4Mask .|. mod1Mask, d) ~> WNSwap D
            , (mod4Mask .|. mod1Mask, r) ~> WNSwap R ]
--      fadeMostInactives = fadeOutLogHook . fadeIf (isUnfocused <&&> noneOf qs)
--      noneOf = fmap not . foldr1 (<||>)
--      qs = [className =? "Gimp", className =? "URxvt",  isFullscreen, ("layer" `isInfixOf`) <$> className]

-- }}}

-- keyboard and mouse {{{

 -- Uses mod4 on win key *and* caps lock
 --     via `Option "XkbOptions" "caps:super"'
 -- Both shift keys pressed together turns on capslock, either one alone turns it off,
 --     via "shift:both_capslock_cancel"

buttons =
    -- keep defaults on buttons 1 & 2:
    -- button3 was the only needed to move and resize, but something has gone
    -- wrong/regressed some while back, slams to a horizontal or
    -- vertical zero when resized, which didn't use to happen.
    [ (mod4Mask             , button3) ~> Flex.mouseWindow Flex.discrete
    , (mod4Mask             , button4) ~> const $ windows W.swapDown
    , (mod4Mask             , button5) ~> const $ windows W.swapUp
    , (mod4Mask .|. mod1Mask, button4) ~> const $ moveTo Next HiddenWS
    , (mod4Mask .|. mod1Mask, button5) ~> const $ moveTo Prev HiddenWS ]


keys = --
    [ "M-C-S-q"       ~> io (exitWith ExitSuccess)
    , "M-<Space>"     ~> sendMessage ToggleLayout  -- toggle fullscreen
    , "M-c"           ~> sendMessage NextLayout
    , "M-<Return>"    ~> windows W.focusMaster
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
    [ "M-<Tab>"   ~> namedScratchpadAction pads "scratch"
    , "M-<F1>"    ~> namedScratchpadAction pads "nautilus"
    , "M-<F2>"    ~> namedScratchpadAction pads "tote"
    , "M-<F3>"    ~> namedScratchpadAction pads "asunder"
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
    [ isDialog                -?> doF W.shiftMaster <+> doFloat
    , ("libreoffice" `isPrefixOf`) <$> className -?> doShift "3"
    , ("Gimp"        `isPrefixOf`) <$> className -?> doShift "5"
    , title =? "Quick Alarm"  -?> doF W.shiftMaster <+> doFloat
    , role =? "reminderFoxEdit" -?>
        doF W.shiftMaster <+> doRectFloat (W.RationalRect 0.15 0.46 0.52 0.432)
    , className =? "Gpick"    -?> doFloat
    , className =? "Firefox"  -?> doShift "8"
    , isMPlayerFull           -?> doShift "6"
    , title =? "Ripping"      -?> doShift "4"
    , className =? "Audacity" -?> doShift "4"
    , className =? "Apvlv"    -?> doShift "2"
    , className =? "Xmessage" -?> doF W.shiftMaster <+> doCenterFloat
    , className =? "feh"      -?> doF W.shiftMaster <+> doFloat
    , className =? ""         -?> doFloat -- low budget gtk windows
    ] <+> transience' <+> manageDocks
  where
    role = stringProperty "WM_WINDOW_ROLE"
    isMPlayerFull = (className =? "MPlayer" <&&> appName =? "gl2")
                        <||> title =? "Gnome MPlayer Fullscreen"
-- }}}

-- layouts {{{

layouts = modifiers
            . onWorkspace "1" four
            . onWorkspace "2" (TallAlt i 0.5 ||| Mirror (Tall 1 i 0.516))
            . onWorkspaces ["3", "4"] (workspaceDir cwd many) . onWorkspace "5" gimp
            . onWorkspace "7" (workspaceDir "~/.xmonad" four) $ workspaceDir "~" many
  where
    cwd  = "/e4/av/Music"
    i = 0.00125
    modifiers = smartBorders . toggleLayouts (noBorders Full)
        . layoutHintsToCenter . spacing 1 . avoidStruts
    four = limitWindows 4 (TallAlt i 0.516) ||| Full
    gimp = workspaceDir "~/images" . reflectHoriz $ withIM 0.145 (Role "gimp-toolbox") four
    many = Mirror (TallAlt i 0.705) ||| Full ||| Tall 1 i 0.591
--  many = Mirror (TallAlt i (31/44)) ||| Full ||| Tall 1 i (13/22)

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

-- }}}

-- prompt and dzen {{{

-- solarized color pallette
solbase03  = "#002b36"
solbase02  = "#073642"
solbase01  = "#586e75"
solbase00  = "#657b83"
solbase0   = "#839496"
solbase1   = "#93a1a1"
solbase2   = "#eee8d5"
solbase3   = "#fdf6e3"
solyellow  = "#b58900"
solorange  = "#cb4b16"
solred     = "#dc322f"
solmagenta = "#d33682"
solviolet  = "#6c71c4"
solblue    = "#268bd2"
solcyan    = "#2aa198"
solgreen   = "#859900"

promptConfig = defaultXPConfig
    { font = "xft:Consolas:12"
    , bgColor  = solbase03
    , fgColor  = solbase1
--  , fgColor  = solbase2
    , bgHLight = solyellow
    , fgHLight = solbase02
    , promptBorderWidth = 0
    , height   = 28
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
    , ppTitle           = take 143 . dzenEscape
    , ppSep             = ""
    , ppSort            = getSortByXineramaRule -- ensure wsIds alphabetical
    , ppOrder           = ppo
    }
  where
    wsIcon = wrap "^i(" ")" . ("/home/gvg/.config/dzen/icons/" ++) . wrap "ws-" ".xbm"
    ppo (ws:_:t:_) = ["^p(18;)", ws, "^p(80;).: ", t, " :."]
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
