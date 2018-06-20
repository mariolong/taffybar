module Main where

import XMonad

import XMonad.Actions.CopyWindow (copyToAll) -- useful for mplayer
import XMonad.Actions.CycleWS
import XMonad.Actions.SpawnOn
-- import XMonad.Config.Desktop
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.InsertPosition -- Focus windows
import XMonad.Hooks.ManageDocks -- This module provides tools to automatically manage dock type programs
import XMonad.Hooks.ManageHelpers
-- import XMonad.Hooks.UrgencyHook
-- import XMonad.Layout.Decoration --(defaultTheme)
import XMonad.Layout.Maximize
import XMonad.Layout.Tabbed
import XMonad.Layout.MultiToggle
import XMonad.Layout.NoBorders (noBorders, smartBorders, lessBorders, Ambiguity (OnlyFloat))
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Reflect
import XMonad.Layout.Spacing()
import XMonad.Layout.Renamed as R

import qualified XMonad.StackSet as W

import XMonad.Util.EZConfig
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.WorkspaceCompare

import System.Taffybar.Hooks.PagerHints (pagerHints)

main::IO()
main = xmonad $
       docks $
       ewmh $
       pagerHints
       def { workspaces = myWorkspaces
           , modMask    = mod4Mask
           , terminal   = "xterm -e 'export SHELL=/usr/bin/fish; export XTERM_SHELL=$SHELL; fish'"
           , focusFollowsMouse = False
           , borderWidth = 2
           , normalBorderColor = "#3d3d3d"
           , focusedBorderColor = "#f99157"
           , layoutHook = myLayoutHook
           , manageHook = myManageHook
           , handleEventHook = docksEventHook <+> fullscreenEventHook -- for google chrome fullscreen
           , startupHook = spawn "taffybar"
           } `additionalKeysP` myKeys

myWorkspaces::[String]
myWorkspaces = map show [1 .. 9 :: Int]

myTabTheme::Theme
myTabTheme = def { inactiveColor = "#222222"
                 , inactiveTextColor = "#777777"
                 , inactiveBorderColor = "#222222"
                 , activeColor = "#555555"
                 , activeTextColor = "#ffdb99"
                 , activeBorderColor = "#ffdb99"
                 , decoHeight = 28
                 , fontName = "xft:Source Han Sans:size=14"}


-- renamed layout
-- https://gist.github.com/tylevad/3146111
myTall nmaster delta ratio = renamed [R.Replace "｜"] $ Tall nmaster delta ratio

myTileV = myTall 1 (1/100) (1/2)
myTileH = renamed [R.Replace "－"] $ Mirror myTileV
myTabbed = renamed [R.Replace "＋"] $ noBorders $ tabbedBottom shrinkText myTabTheme

--remove borders (only) from floating windows covering the full screen
--http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Layout-NoBorders.html
--lessBorders OnlyFloat

myLayoutHook = avoidStruts $
               smartBorders $
               lessBorders OnlyFloat $
               onWorkspace "1" myLayoutEmacs $
               onWorkspace "3" myLayoutEmacs $
               onWorkspace "6" myLayoutFileManager $
               onWorkspace "9" myLayoutTerm
               myLayoutDefault

-- change layout name
-- https://hackage.haskell.org/package/xmonad-contrib-0.13/docs/XMonad-Layout-Renamed.html
-- myLayoutHook = renamed [PrependWords "Awesome"] $ tiled ||| Mirror tiled ||| Full
myLayoutDefault = myTileV ||| myTileH ||| myTabbed

myLayoutEmacs = myTabbed ||| myTileV

myLayoutFileManager = myTileV ||| myTabbed

myLayoutTalk = myTall 1 (2/100) (2/7)

myLayoutTerm = myMirrorTiledTerm ||| myTiledTerm ||| myTabbed

myTiledTerm = myTall 1 (5/100) (5/7)

myMirrorTiledTerm = renamed [R.Replace "－"] $ Mirror myTiledTerm


myKeys :: [([Char], X ())]
myKeys = [ ("M-S-q",     spawnHere "oblogout")
         , ("M-C-l",     spawn "sleep 0.5; xset dpms force off")

         -- applications
         , ("M-x M-b",   spawnHere "google-chrome-stable")
         , ("M-x M-e",   spawnHere "emacsclient -c -a ''")
         , ("M-x M-d",   spawnHere "pcmanfm-qt")
         , ("M-x M-t",   spawnHere "qbittorrent")
         , ("M-x M-k",   spawnHere "kodi")
         , ("M-x M-f",   spawnHere "xterm -name mc -e 'export SHELL=/usr/bin/fish;export XTERM_SHELL=$SHELL; mc'")
         , ("M-c c",     spawnHere "emacsclient -ne '(make-capture-frame)'")
         , ("M-x M-m",   spawnHere "st -n ncmpcpp -e ncmpcpp")
         , ("C-<Esc>",   spawnHere "st -n htop -e htop")

         -- music player control
         , ("M-m M-m",   spawn "mpc toggle")
         , ("M-m M-p",   spawn "mpc prev")
         , ("M-m M-n",   spawn "mpc next")
         , ("M-m M-d",   spawn "mpc volume -3")
         , ("M-m M-u",   spawn "mpc volume +3")
         , ("M-m M-h",   spawn "mpc volume 100")
         , ("M-m M-j",   spawn "mpc volume 80")
         , ("M-m M-k",   spawn "mpc volume 60")
         , ("M-m M-l",   spawn "mpc volume 40")

         -- volume control
         , ("<XF86AudioMute>", spawn "amixer -q sset Master toggle")
         , ("<XF86AudioLowerVolume>", spawn "amixer -q sset Master 3%-")
         , ("<XF86AudioRaiseVolume>", spawn "amixer -q sset Master 3%+")

         -- call calculator
         , ("<XF86Calculator>", spawn "speedcrunch")

         -- recompile & restart xmonad
         -- , ("M-q",       notifySpawn "xmonad --recompile && xmonad --restart" "Re-compile and restart XMonad")
         , ("M-q",       notifySpawn "xmonad --restart" "Restart XMonad")

         -- switch desktop pictures
         , ("M-s", spawn "systemctl start wallpaper --user")

         -- print screen
         , ("<Print>",   spawn "import -window root ~/Pictures/$(date +%Y%m%d-%H%M%S).png")
         , ("M-<Print>", spawn "import ~/Pictures/$(date +%Y%m%d-%H%M%S).png")

         -- workspace navigation
         , ("M-f",       moveTo Next EmptyWS)
         , ("M-<R>",     moveTo Next HiddenNonEmptyWS)
         , ("M-<L>",     moveTo Prev HiddenNonEmptyWS)

         -- windown navigation
         , ("M-<U>",     windows W.focusUp)
         , ("M-<D>",     windows W.focusDown)

         -- move focus window to other workspace
         , ("S-M-f",     followTo Next EmptyWS)
         , ("S-M-<D>",   followTo Next HiddenNonEmptyWS)
         , ("S-M-<U>",   followTo Prev HiddenNonEmptyWS)

         -- sticky window
         -- , ("M-S-s",     windows copyToAll)

         -- left/right switch
         , ("S-M-x",     sendMessage $ Toggle REFLECTX)

         -- up/down switch
         , ("S-M-y",     sendMessage $ Toggle REFLECTY)

         -- switch between current WS and previous WS
         , ("M-z",       toggleWS)

         -- switch fullscreen
         , ("M-\\",      withFocused (sendMessage . maximizeRestore))
         ]


myManageHook :: ManageHook
myManageHook = manageApps
               <+> insertPosition Above Newer -- default above newer is ok.
               <+> composeOne[ fmap not isDialog -?> doF avoidMaster
                             , return True -?> doF W.swapDown]
               <+> composeOne[ isFullscreen -?> doFullFloat
                             , isDialog -?> doCenterFloat <+> doF W.swapUp]

manageApps :: ManageHook
manageApps = composeOne . concat $
    [ [className =? c -?> doFloatToAll | c <- appFloatToAll ]
    , [className =? c -?> doFullToAll | c <- appFullToAll]
    , [className =? c -?> doCenterFloat | c <- appFloats]
    , [role =? "browser" -?> doShiftAndGo "1"]
    , [appName =? "crx_knipolnnllmklapflnccelgolnpehhpl" -?> doShiftAndGo "2"]
    , [appName =? "emacs" -?> doShiftAndGo "3"]
    , [appName =? "htop" -?> doShiftAndGo "5"]
    , [appName =? c -?> doShiftAndGo "6" | c <- appFileManager]
    , [appName =? c -?> doShiftAndGo "7" | c <- appMedias]
    , [appName =? "qbittorrent" -?> doShiftAndGo "8"]
    , [appName =? c -?> doShiftAndGo "9" | c <- appTerms]
    , [title =? "alsa-tray" -?> doFloat]
    ]
    where
      role = stringProperty "WM_WINDOW_ROLE"
      doFloatToAll = doFloat <+> doF copyToAll
      doFullToAll = doFullFloat <+> doF copyToAll
      doShiftAndGo ws = doF (W.greedyView ws) <+> doShift ws -- open on workspace ws then switch to ws
      appFloats = ["SpeedCrunch"]
      appFloatToAll = ["mpv", "Mplayer"]
      appFullToAll = ["Oblogout", "slock"]
      appFileManager = ["mc", "pcmanfm-qt"]
      appMedias = ["Kodi", "ncmpcpp", "gwenview"]
      appTerms = ["xterm", "st-256color" ]


-- Written by Marshall Lochbaum on xmonad@haskell.org mailing list
-- https://markmail.org/message/adewkowzezvqn4wc#query:+page:1+mid:j4fa7fvmm5wg6aze+state:results
-- The goal is to move the window to the next empty workspace and then to follow it there.
followTo :: Direction1D -> WSType -> X ()
followTo dir t = doTo dir t getSortByIndex (\w -> (windows (W.shift w)) >> (windows (W.greedyView w)))

{- Section 3 of http://www.haskell.org/haskellwiki/Xmonad/Frequently_asked_questions
   Note that this is not a good solution for people who use floating
   windows, since many operations on floats put the floating window
   into the master position. Some transient windows will be
   swappedDown to appear below the floating parent unless the user
   keeps a tiled window in master and floating windows lower in the
   stack at all times. As with swapDown it's best to use it only on
   specific windows or at the end of a composeOne list if you use
   floating windows very often.  and, in particular, including the
   tweak to keep focus with the master window -}
avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c -> case c of
                                  W.Stack t [] (r:rs) ->  W.Stack r [] (t:rs)
                                  otherwise -> c


notifyLaunch :: MonadIO m => (t -> m b) -> t -> String -> m b
notifyLaunch f a n = do
  safeSpawn "notify-send" [" ", n]
  f a

notifySpawnHere::String->String-> X ()
notifySpawnHere app note = notifyLaunch spawnHere app note

notifySpawn::String->String-> X ()
notifySpawn app note = notifyLaunch spawn app note
