{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE PackageImports #-}

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Monoid (mconcat)
import qualified Data.Text as T

import qualified Graphics.UI.Gtk as Gtk
import Data.Time.Format

import System.Information.CPU
import System.Information.Memory
import System.Information.Network (getNetInfo)
import System.Taffybar
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.MPRIS2 (mpris2New)
import System.Taffybar.Pager
import System.Taffybar.SimpleClock
import System.Taffybar.Systray
import System.Taffybar.Widgets.PollingGraph
import System.Taffybar.TaffyPager

import XMonad

main :: IO ()
main = do
  defaultTaffybar defaultTaffybarConfig
    {
      startWidgets = [apps, pager]
    , endWidgets = [clock, tray, net, mem, cpu, mpris, note]
    , barPosition = Top
    , barHeight = 32
    , widgetSpacing = 1
    } where timeLocale = Just myTimeLocale
            apps = appLauncherNew
            pager = taffyPagerNew myPagerConfig
            note = notifyAreaNew noteConfig
            clock = textClockNew timeLocale fmtClock 1
            tray = systrayNew
            net = myNetMonitor netCfg 1 "enp3s0" (35*1024*1024/8)
            mem = pollingGraphNew memCfg 1 memCallback
            cpu = pollingGraphNew cpuCfg 1 cpuCallback
            mpris = mpris2New



fmtClock :: String
fmtClock = "<span weight='bold'>"
  ++ "<span fgcolor='#5599ff'> %m/%d (%a)</span>"
  ++ "<span fgcolor='orange'> %H:%M:%S</span>"
  ++ "</span>"


myTimeLocale :: TimeLocale
myTimeLocale = defaultTimeLocale
  {
    wDays = [("週日", "日"), ("週一", "一"),
             ("週二", "二"), ("週三", "三"),
             ("週四", "四"), ("週五", "五"),
             ("週六", "六")]
  }


myGraphConfig :: GraphConfig
myGraphConfig = defaultGraphConfig
  { graphLabel = Nothing
  , graphWidth = 45
  , graphPadding = 1
  , graphBorderColor = (0.17, 0.17, 0.17)
  , graphBackgroundColor = (0.01, 0.01, 0.01)
  }


memCfg :: GraphConfig
memCfg = myGraphConfig
  {
    graphDataColors = [(22/255, 164/255, 41/255, 1)]
  }


cpuCfg :: GraphConfig
cpuCfg = myGraphConfig
  {
    graphDataColors = [(251/255, 184/255, 41/255, 1), (255/255, 100/255, 20/255, 1)]
  }


netCfg :: GraphConfig
netCfg = myGraphConfig
  {
    graphDataColors = [(22/255, 122/255, 184/255, 1), (22/255, 122/255, 250/255, 1)]
  }

memCallback :: IO [Double]
memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]


cpuCallback :: IO [Double]
cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]


netCallback :: IORef [Integer] -> String -> Double -> Double -> IO [Double]
netCallback ref interface interval maxspeed = do
  maybeThisSample <- getNetInfo interface

  case maybeThisSample of
    Nothing -> return [0, 0]
    Just thisSample -> do
      lastSample <- readIORef ref
      writeIORef ref thisSample
      return $ calcSpeed thisSample lastSample interval maxspeed


calcSpeed :: [Integer] -> [Integer] -> Double -> Double -> [Double]
calcSpeed thisSample lastSample interval maxspeed =
    map (/ (interval*maxspeed)) $ map fromInteger $ zipWith (-) thisSample lastSample


myNetMonitor :: GraphConfig -> Double -> String -> Double -> IO Gtk.Widget
myNetMonitor cfg interval interface maxspeed = do
  ref <- newIORef []
  pollingGraphNew cfg interval $ netCallback ref interface interval maxspeed


-- borrow from https://github.com/bgamari/xmonad-config/blob/master/taffybar/taffybar.hs
myFormatter :: Notification -> String
myFormatter note = msg
  where
    msg = case T.null (noteBody note) of
      True  -> T.unpack $ noteSummary note
      False -> T.unpack $ mconcat [ "<span fgcolor='orange'>● </span>"
                                  , noteSummary note, ""
                                  , head $ T.lines $ noteBody note ]


noteConfig :: NotificationConfig
noteConfig = defaultNotificationConfig
  {
    notificationMaxLength = 65
  , notificationMaxTimeout = 5
  , notificationFormatter = myFormatter
  }


myPagerConfig :: PagerConfig
myPagerConfig = defaultPagerConfig
  {
    activeWindow     = escape . shorten 30
  , activeWorkspace  = colorize "#ffffff" "#000000" .escape
  , activeLayout     = colorize "orange" "#333333" . escape
  , widgetSep        = "  "
  }


appLauncherNew :: IO Gtk.Widget
appLauncherNew = do

  box <- Gtk.hBoxNew False 0

  execApp box "/home/mario/.config/taffybar/icon/shutdown.png"
              "oblogout"

  execApp box "/home/mario/.config/taffybar/icon/google-chrome.png"
              "google-chrome-stable"

  -- execApp box "/home/mario/.config/taffybar/icon/file-manager.png"
              -- "pcmanfm-qt"

  -- execApp box "/home/mario/.config/taffybar/icon/network.png"
              -- "st -e 'systemctl start adsl'"

  Gtk.widgetShowAll box
  return (Gtk.toWidget box)


execApp :: Gtk.HBox -> String -> String -> IO ()
execApp box fn cmd = do
  evb <- Gtk.eventBoxNew
  img <- Gtk.imageNewFromFile fn

  Gtk.containerAdd evb img

  evb `Gtk.on` Gtk.buttonPressEvent $ Gtk.tryEvent $ do spawn cmd
    -- Gtk.onButtonPress evb (\x -> if (Gtk.eventButton x) == Gtk.LeftButton
    --                                 then do spawn cmd
    --                                         return (Gtk.eventSent x)
    --                                 else return (Gtk.eventSent x))

  Gtk.boxPackStart box evb Gtk.PackNatural 4
