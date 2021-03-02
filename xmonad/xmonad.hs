--------------------------------------------------------------------------------
-- shunsk's base xmonad.hs file for custamize
-- https://ok-xmonad.blogspot.com
--------------------------------------------------------------------------------

import System.Exit

import XMonad
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import qualified XMonad.StackSet as W
import XMonad.Util.Loggers
import XMonad.Util.EZConfig

---------------------------------------------------------------
-- MAIN
---------------------------------------------------------------
main = do

  -- xmobarを使う
  h <- spawnPipe "xmobar -f 'xft:IPAGothic:size=14:medium:antialias=true'"
  
  -- xmonadの実行
  xmonad
    $ docks 
    $ def { terminal          = "kitty"
          , modMask           = mod4Mask
          , workspaces        = map show [1..5 ::Int]
          , layoutHook        = avoidStruts (layoutHook def)
          , logHook           = myLogHook h
          , keys              = \c -> mkKeymap c (keyMapDataList c)
          }

---------------------------------------------
-- ステータスバー表示のカスタマイズ
---------------------------------------------

myLogHook h =
  dynamicLogWithPP def
      { ppOutput  = hPutStrLn h
      , ppCurrent = wrap "[" "]"
      , ppHidden   = id
      , ppHiddenNoWindows = const ""
      , ppUrgent  = id
      , ppSep     = " : "
      , ppWsSep   = " "
      , ppTitle   = shorten 80
      , ppTitleSanitize = xmobarStrip . dzenEscape
      , ppLayout  = id
      , ppOrder   = id
      , ppSort    = getSortByIndex
      , ppExtras  = []
      }


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
  ,("M-S-q", io (exitWith ExitSuccess))
  ,("M-q", spawn myRecompileCmd)
  ]
  -- workspaceの移動等
  ++
  [("M-" ++ m ++ show k , windows $ f i)
    | (i,k) <- zip (XMonad.workspaces conf) ([1..5] :: [Int])
    , (f,m) <- [(W.view, ""),(W.shift, "S-")]
  ]
  where
    myRecompileCmd =
      "xmonad --recompile && (killall xmobar; xmonad --restart)"
