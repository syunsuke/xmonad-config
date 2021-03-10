------------------------------------------------------------------------------
-- shunsk's base xmonad.hs file for custamize
-- https://ok-xmonad.blogspot.com

-- Navigation2Dモジュール説明用

-- 説明で使うxmonad.hsのベースになるファイル
-- ~/.config/xmobar/mobarrcはリネームしておく
-- xmobarのフォントは日本語が使えるIPAフォント
-- 必要アプリ
--  kitty
--  dmenu
--  IPAfont
--  xmobar
------------------------------------------------------------------------------

import System.Exit

import XMonad
import XMonad.Util.Run
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import qualified XMonad.StackSet as W

import XMonad.Util.EZConfig

-- 方向の指示でフォーカス移動する
import XMonad.Actions.Navigation2D

---------------------------------------------------------------
-- MAIN
---------------------------------------------------------------
main = do

  -- xmobarを使う
  h <- spawnPipe "xmobar -f 'xft:IPAGothic:size=14:medium:antialias=true'"
  
  -- xmonadの実行
  xmonad
    $ docks
    $ withNavigation2DConfig myNavi2dConfig
    $ def { terminal          = "kitty"
          , modMask           = mod4Mask
          , focusFollowsMouse = False
          , workspaces        = map show [1..5 ::Int]
          , manageHook        = manageHook def
          , handleEventHook   = handleEventHook def
          , layoutHook        = mylayouthook
          , logHook           = myXmobarLogHook h
          , keys              = \c -> mkKeymap c (keyMapDataList c)
          }

---------------------------------------------
-- Navigation2DConfig型データの調整
---------------------------------------------
myNavi2dConfig 
  = def { defaultTiledNavigation = sideNavigation
        }

---------------------------------------------
-- ステータスバー表示のカスタマイズ
---------------------------------------------

-- xmobar用
myXmobarLogHook h =
  dynamicLogWithPP
    xmobarPP 
      { ppOutput  = hPutStrLn h 
      , ppCurrent = xmobarColor "#FF9F1C" "#1A1B41" . pad . wrap "[" "]" 
      , ppTitle   = xmobarColor "#1A1B41" "#C2E7DA" . shorten 50 . pad
      }

---------------------------------------------
-- レイアウト
---------------------------------------------
mylayouthook = 
  avoidStruts mytall ||| avoidStruts mymirror ||| myfull
  where mytall   =  Tall 1 0.03 0.5
        mymirror =  Mirror mytall
        myfull   =  Full

---------------------------------------------
-- キーバインド関連
---------------------------------------------
keyMapDataList :: XConfig Layout -> [(String, X ())]
keyMapDataList conf =
  [("M-S-<Return>", spawn $ XMonad.terminal conf)
  ,("M-p", spawn "dmenu_run")
  ,("M-S-c", kill)
  ,("M-<Space>", sendMessage NextLayout)
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

  -- Navigation2D用のキーバインド
  ,("M-<R>", windowGo R False)
  ,("M-<L>", windowGo L False)
  ,("M-<U>", windowGo U False)
  ,("M-<D>", windowGo D False)
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
