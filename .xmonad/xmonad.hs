{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-} -- for TallAlt

-- sereven xmonad.hs (0.9.2 - 0.10) $ 2011-03-19

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
import XMonad.Layout.LayoutScreens
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
import XMonad.Util.Scratchpad
import XMonad.Util.WorkspaceCompare
-- }}}

wsIds = map return "123456789" ++ ["NSP"]

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

-- keyboard and mouse {{{
-- uses mod4 on logo- key and caps-lock via ` Option "XkbOptions" "caps:super" '

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

keys =
    [ "M-M1-S-q"      ~> io (exitWith ExitSuccess)
    , "M-S-q"         ~> spawn "xmessage -- [ Shift+Mod1+Mod4+Q to exit; no gnome-session mgr] --"
    , "M-<Space>"     ~> sendMessage ToggleLayout  -- toggle fullscreen
    , "M-c"           ~> sendMessage NextLayout
    , "M-M1-."        ~> sendMessage Shrink
    , "M-M1-,"        ~> sendMessage Expand
    , "M-<F9>"        ~> layoutScreens 3 $ fixedLayout
                            [Rectangle 0 0 1600 1200, Rectangle 1600 0   1600 480
                                                    , Rectangle 1600 480 1280 720]
    , "M-<F8>"        ~> rescreen
    ]
    -- workspaces and screens -- 1 2 3 \
                              --  q w e \
                              --   a s d \ f g
    ++                        --          \ v b
    [ mask ++ [key] ~> action i
         | (key, i) <- zip "123qweasd=" wsIds
         , (mask, action) <- [ ("M-", toggled W.greedyView)
                             , ("M-M1-", toggled followShift) ] ]
    ++
    [ mask ++ [key] ~> screenWorkspace s >>= flip whenJust (windows . action)
         | (key, s) <- zip "fgb" [0..]
         , (mask, action) <- [ ("M-", W.view)
                             , ("M-M1-", W.shift) ] ]
    ++
    [ "M-v"         ~> swapNextScreen
    -- scratch term, prompts and searches
    , "M-<Tab>" ~> scratchpadSpawnActionTerminal "urxvt -pe tabbed"
    , "M-r"     ~> runOrRaisePrompt promptConfig
    , "M-M1-r"  ~> changeDir promptConfig ]
    ++
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

--use intelligent
-- manage hook {{{
manageHooks =
    composeAll
    [ scratchpadManageHook $ W.RationalRect 0.43 0.575 0.53 0.34, manageDocks ]
    <+>
    composeOne
    [ className =? ""         -?> doFloat -- low budget gtk windows
    , ("OpenOffice" `isPrefixOf`) <$> className -?> doShift "3"
    , ("Gimp" `isPrefixOf`)       <$> className -?> doShift "5"
    , className =? "Cryptote"
            -?> doF W.shiftMaster <+> (doRectFloat $ W.RationalRect 0.1 0.6 0.5 0.35)
    , className =? "Acroread" -?> doShift "2"
    , className =? "Xmessage" -?> doF W.shiftMaster <+> doCenterFloat
    , className =? "Qjackctl" -?> doFloat
    , className =? "feh"      -?> doFloat
    , className =? "XFontSel" -?> doF W.shiftMaster <+> doCenterFloat
    , transience
    , isDialog -?> doF W.shiftMaster <+> doFloat
    , return True -?> doF W.swapDown
    ]
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
    modifiers . onWorkspaces (take 4 wsIds) (workspaceDir cwd many) . onWorkspace "5" gimp
              . onWorkspace "7" (workspaceDir "~/.xmonad" four) $ workspaceDir "~" many
  where
    modifiers =
        smartBorders . toggleLayouts (noBorders Full) . layoutHintsToCenter . avoidStruts
    four = limitWindows 4 . Mirror $ TallAlt 0.03 (31/44) ||| Full
    gimp = workspaceDir "~/images" . reflectHoriz $ withIM 0.145 (Role "gimp-toolbox") four
    many = Mirror $ TallAlt 0.03 (31/44) ||| Full ||| Tall 1 0.03 (13/22)
    cwd  = "~/cet_spr/phy/labs"
-- }}}

-- prompt and dzen {{{

promptConfig = defaultXPConfig
    { font = "xft:Denmark:Thin:size=11"
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
      ++ " -fn " ++ "'" ++ drop 4 (font promptConfig) ++ "'" -- dzen doesn't use xft prefix
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
    , ppOrder           = \(ws:_:t:_) ->  ["^p(18;)", ws, "^p(30;).: ", t, " :."]
    }
  where
    wsIcon = wrap "^i(" ")" . ("/home/gvg/.config/dzen/icons/" ++) . wrap "ws-" ".xbm"

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
