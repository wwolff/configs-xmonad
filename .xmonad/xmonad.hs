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
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowNavigation
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.LayoutHints
import XMonad.Layout.LimitWindows
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
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
    , NS "cryptote" "cryptote" (resource =? "cryptote") cryptoteHook
    , NS "keepassx" "keepassx" (title =? "KeePassX - Password Manager") keepassxHook
    ]
  where
    asunderQuery = ("Asunder" `isPrefixOf`) <$> title
    asunderHook  = doRectFloat $ rr 0.61 0.156 0.3 0.72
    scratchHook  = doRectFloat $ rr 0.38 0.07 0.508 0.3
    nautilusHook = doRectFloat $ rr 0.45 0.19 0.5 0.65
    cryptoteHook = doRectFloat $ rr 0.1 0.36 0.6 0.58
    keepassxHook = doRectFloat $ rr 0.49 0.03 0.51 0.52
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
            , manageHook         = manageHooks
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
    , "M-<F2>"    ~> namedScratchpadAction pads "cryptote"
    , "M-M1-<F2>" ~> namedScratchpadAction pads "keepassx"
    , "M-<F3>"    ~> namedScratchpadAction pads "asunder"
    , "M-r"       ~> runOrRaisePrompt promptConfig
    , "M-M1-r"    ~> changeDir promptConfig ]
  where
    toggled = toggleOrDoSkip ["NSP"]
    followShift = liftM2 (.) W.view W.shift

-- }}}

-- manage hook {{{
manageHooks = namedScratchpadManageHook pads <+> composeOne
    [ isDialog                -?> doF W.shiftMaster <+> doFloat
    , ("libreoffice" `isPrefixOf`) <$> className -?> doShift "3"
    , ("Gimp"        `isPrefixOf`) <$> className -?> doShift "5"
    , title =? "Quick Alarm"  -?> doF W.shiftMaster <+> doFloat
    , role =? "reminderFoxEdit" -?>
        doF W.shiftMaster <+> doRectFloat (W.RationalRect 0.15 0.46 0.52 0.432)
    , className =? "Gpick"    -?> doFloat
    , className =? "Wuala"    -?> doShift "NSP"
    , className =? "Audacity" -?> doShift "9"
    , title     =? "Ripping"  -?> doShift "9"
    , className =? "Firefox"  -?> doShift "8"
    , isMPlayerFull           -?> doShift "6"
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
            . onWorkspaces ["1", "4"] (workspaceDir "~" edit)
            . onWorkspaces ["2", "3"] (workspaceDir "~/doc" doc)
            . onWorkspace "5" (workspaceDir "~/images" edit)
            . onWorkspace "7" (workspaceDir "~/.xmonad" edit) $ workspaceDir cwd many
  where
    cwd  = "/e4/av/Music"
    modifiers = smartBorders . toggleLayouts (noBorders Full)
        . layoutHintsToCenter . spacing 1 . avoidStruts
    doc = limitWindows 6 $ TallAlt i 0.5 ||| Mirror (Tall 1 i 0.516)
    edit = limitWindows 4 (TallAlt i 0.516) ||| Full
    many = Mirror (TallAlt i 0.705) ||| Full ||| Tall 1 i 0.591
    i = 0.00125

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
