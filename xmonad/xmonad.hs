--------------------------------------------------------------------------------
-- shunsk's base xmonad.hs file for dynamicproject
-- https://ok-xmonad.blogspot.com
--
-- 必要なプログラム
--   kitty(ターミナルプログラム)
--   xmobar
--   dmenu
--   chromium
--   IPAGothic font
--------------------------------------------------------------------------------

import System.Exit

import XMonad
import XMonad.Util.Run
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import qualified XMonad.StackSet as W

import XMonad.Util.EZConfig
import XMonad.Actions.DynamicProjects
import XMonad.Actions.CycleWS
import XMonad.Prompt
---------------------------------------------------------------
-- MAIN
---------------------------------------------------------------
main = do

  -- xmobarを使う
  h <- spawnPipe "xmobar -f 'xft:IPAGothic:size=14:medium:antialias=true'"
  
  -- xmonadの実行
  xmonad
    $ dynamicProjects projects
    $ docks 
    $ def { terminal          = "kitty"
          , modMask           = mod4Mask
          , focusFollowsMouse = False
          , workspaces        = ["home"] 
          , manageHook        = manageHook def
          , handleEventHook   = handleEventHook def
          , layoutHook        = avoidStruts $ layoutHook def
          , logHook           = myXmobarLogHook h
          , keys              = \c -> mkKeymap c (keyMapDataList c)
          }

---------------------------------------------
-- ステータスバー表示のカスタマイズ
---------------------------------------------
myXmobarLogHook h =
  dynamicLogWithPP
    xmobarPP 
      { ppOutput  = hPutStrLn h 
      , ppCurrent = xmobarColor "#FF9F1C" "#1A1B41" . pad . wrap "[" "]" 
      , ppTitle   = xmobarColor "#1A1B41" "#C2E7DA" . shorten 50 . pad
      }

---------------------------------------------
-- ワークスペース入力プロンプトの見栄え
---------------------------------------------
promptAethetic = def
    { font    = "xft:IPAGothic:size=16:medium:antialias=true"
    , position = CenteredAt 0.5 0.8 
    , height   = 40
    }

---------------------------------------------
-- DynamicProcect 用のアクション
---------------------------------------------
projects :: [Project]
projects =
  [ Project   "home"    "~/"          Nothing
  , Project   "config"  "~/.config"   Nothing
  , Project   "web"     "~/"          (Just $ spawn "chromium")
  , Project   "code"    "~/code"      (Just $ spawn "kitty")

  
  -- Archlinux page for application list 
  , Project   
      "applist" 
      "~/"
      (Just $ spawn $ "chromium " ++ url_applist ++ " --new-window")
  ]
   where 
      url_applist = "https://wiki.archlinux.org/index.php/List_of_applications"

---------------------------------------------
-- キーバインド関連
---------------------------------------------
keyMapDataList :: XConfig Layout -> [(String, X ())]
keyMapDataList conf =
  [("M-S-<Return>", spawn $ XMonad.terminal conf)
  ,("M-p", spawn "dmenu_run")
  ,("M-S-c", kill)
  ,("M-<Space>", sendMessage NextLayout)
  ,("M-S-<Space>", setLayout $  XMonad.layoutHook conf)
  ,("M-n", refresh)
  ,("M-j", windows W.focusDown)
  ,("M-k", windows W.focusUp)
  ,("M-m", windows W.focusMaster)
  ,("M-S-j", windows W.swapDown)
  ,("M-S-k", windows W.swapUp)
  ,("M-<Return>", windows W.swapMaster)
  ,("M-t", withFocused $ windows . W.sink)
  ,("M-S-q", io (exitWith ExitSuccess))
  ,("M-q", spawn myRecompileCmd)

   -- カーソルキーによるworkspaceの移動
  ,("M-<R>", moveTo Next HiddenNonEmptyWS)
  ,("M-<L>", moveTo Prev HiddenNonEmptyWS)
  ,("M-C-<R>", moveTo Next AnyWS)
  ,("M-C-<L>", moveTo Prev AnyWS) 

  -- プロンプトによるワークスペース移動
  ,("M-g",   switchProjectPrompt promptAethetic)
  ,("M-S-g", shiftToProjectPrompt promptAethetic)
  ]
  where
    myRecompileCmd =
      "xmonad --recompile && (killall xmobar; xmonad --restart)"
