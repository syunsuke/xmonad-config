--------------------------------------------------------------------------------
-- shunsk's base xmonad.hs file for NamedScratchPad
-- https://ok-xmonad.blogspot.com
--
-- 必要な外部アプリ
--   kittyターミナル
--   dmenu
--------------------------------------------------------------------------------

import System.Exit
import XMonad
import XMonad.Util.Run
import XMonad.Util.EZConfig

import XMonad.Util.NamedScratchpad
import XMonad.ManageHook
import qualified XMonad.StackSet as W

---------------------------------------------------------------
-- MAIN
---------------------------------------------------------------
main = xmonad
      def { terminal          = "kitty"
          , modMask           = mod4Mask

          -- スクラッチパッドを操作出来るように
          -- manageHookを修正する
          , manageHook        = manageHook def
                              <+> namedScratchpadManageHook mySPconf
          
          , keys              = \c -> mkKeymap c (keyMapDataList c)
          }

---------------------------------------------
-- スクラッチパッド設定データの作成
---------------------------------------------

mySPconf = [
  NS  "ScrathcPad01" 
      "kitty -T ScratchPad01"
      (title =? "ScratchPad01")
      (customFloating $ W.RationalRect 0.51 0.05 0.48 0.5),
  
  NS  "ScrathcPad02" 
      "kitty --class ScratchPad02"
      (className =? "ScratchPad02")
      defaultFloating, 

  NS  "ScrathcPad03" 
      "kitty -T ScratchPad03 htop"
      (title =? "ScratchPad03")
      nonFloating
  ]

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
  ]

  ++
  [("M-" ++ m ++ show k , windows $ f i)
    | (i,k) <- zip (XMonad.workspaces conf) ([1..5] :: [Int])
    , (f,m) <- [(W.view, ""),(W.shift, "S-")]
  ]
  
  -- スクラッチパッドの呼び出しキー
  ++
  [("M-o", namedScratchpadAction mySPconf "ScrathcPad01")
  ,("M-i", namedScratchpadAction mySPconf "ScrathcPad02")
  ,("M-S-i", namedScratchpadAction mySPconf "ScrathcPad03")
  ]
  
  where
    myRecompileCmd =
      "xmonad --recompile && (killall xmobar; xmonad --restart)"
