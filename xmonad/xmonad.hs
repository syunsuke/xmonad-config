-- xmonad config used by 
-- https://github.com/syunsuke/xmonad-config

import System.Exit
import Control.Concurrent

import XMonad
import XMonad.Util.Run
import XMonad.ManageHook

import XMonad.Layout.Spacing
import XMonad.Layout.Renamed
import XMonad.Layout.LayoutModifier
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops

import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare
import XMonad.Prompt.Workspace
import XMonad.Prompt

import XMonad.Actions.CycleWS
import XMonad.Actions.TopicSpace
import XMonad.Actions.DynamicProjects
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO

import qualified Data.Map as M
import qualified XMonad.StackSet as W

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8


---------------------------------------------------------------
-- MAIN
---------------------------------------------------------------
main = do

  -- Request access to the DBus name
  dbus <- D.connectSession
  D.requestName dbus (D.busName_ "org.xmonad.Log")
    [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  
  -- xmonadの実行
  xmonad 
    $ dynamicProjects projects
    $ docks 
    $ def { terminal          = "kitty"
          , modMask           = mod4Mask
          , focusFollowsMouse = False
          , workspaces        = ["home"]
          , borderWidth　     = 3
          , normalBorderColor = "#cccccc"
          , focusedBorderColor= "#00bbff"
          
          , manageHook        
              = namedScratchpadManageHook mySPConf 
                <+> myManageHook

          , handleEventHook
              = fullscreenEventHook 
                <+> handleEventHook def
          
          , layoutHook        = mylayouthook
          
          , logHook 
              = dynamicLogWithPP 
                . namedScratchpadFilterOutWorkspacePP 
                $ myPolybarPP dbus

          , keys = \c -> mkKeymap c (myKeyMap c)
          }

---------------------------------------------------------------
-- ウィンドウ配置
---------------------------------------------------------------
myManageHook 
  = composeAll 
  $ [ className =? "peek" --> doFloat,
      manageHook def
    ]


---------------------------------------------------------------
-- レイアウト
---------------------------------------------------------------
mylayouthook 
  = smartBorders $ avoidStruts (mytall ||| mymirror) ||| myfull
  where 
    mytall 
      = renamed [CutWordsLeft 1] 
      $ spacingRaw True (Border 10 10 10 10) True (Border 5 5 5 5) True 
      $ Tall 1 0.03 0.5
    
    mymirror 
      = Mirror mytall
    
    myfull
      = noBorders 
      $ Full

------------------------------------------------------------------------
-- Polybar settings (needs DBus client).
------------------------------------------------------------------------

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str =
  let opath  = D.objectPath_ "/org/xmonad/Log"
      iname  = D.interfaceName_ "org.xmonad.Log"
      mname  = D.memberName_ "Update"
      signal = D.signal opath iname mname
      body   = [D.toVariant $ UTF8.decodeString str]
  in  D.emit dbus $ signal { D.signalBody = body }

-- polybar用の見栄え設定
myPolybarPP :: D.Client -> PP
myPolybarPP dbus =
  def { ppOutput  = dbusOutput dbus
      , ppCurrent =  polybarColor "#FF9F1C" "#1A1B41" . wrap "[" "]"
      , ppTitle = wrap "%{R}" "%{R-}" . pad . shorten 30 
      }

-- 色設定のヘルパー関数
-- polybarのタグに関しては
-- https://github.com/polybar/polybar/wiki/Formatting
polybarColor :: String -> String -> String -> String
polybarColor fore_color back_color contents
  = wrap ("%{B" <> back_color <> "} ") " %{B-}" 
  . wrap ("%{F" <> fore_color <> "} ") " %{F-}" 
  $ contents 


---------------------------------------------
-- キーバインド関連
---------------------------------------------
myKeyMap :: XConfig Layout -> [(String, X ())]
myKeyMap conf =
  [("M-S-<Return>", spawn $ XMonad.terminal conf)
  ,("M-p", spawn "rofi -show drun")
  ,("M-S-c", kill)
  ,("M-<Space>", sendMessage NextLayout)
  ,("M-n", refresh)
  
  ,("M-j", windows W.focusDown)
  ,("M-k", windows W.focusUp)
  ,("M-m", windows W.focusMaster)
  ,("M-S-j", windows W.swapDown)
  ,("M-S-k", windows W.swapUp)
  ,("M-<Return>", windows W.swapMaster)
  ,("M-h", sendMessage Shrink)
  ,("M-l", sendMessage Expand)
  ,("M-,", sendMessage $ IncMasterN 1)
  ,("M-.", sendMessage $ IncMasterN (-1))

  ,("M-t", withFocused $ windows . W.sink)

  ,("M-S-q", io (exitWith ExitSuccess))
  ,("M-q", spawn myRecompileCmd)
  
  -- workspaceの移動等
  --,("M-<R>", moveTo Next HiddenNonEmptyWS)
  ,("M-<R>", nextNonEmptyWS)
  ,("M-<L>", prevNonEmptyWS)
  ,("M-C-<R>", moveTo Next AnyWS)
  ,("M-C-<L>", moveTo Prev AnyWS)
  ,("M-g",   switchProjectPrompt myXPConfig)
  ,("M-S-g", shiftToProjectPrompt myXPConfig)
  
  -- スクラッチパッド
  ,("M-o", namedScratchpadAction mySPConf "sp01")
  ,("M-i", namedScratchpadAction mySPConf "sp02")
 
  ,("M-f", sendMessage $ Toggle FULL)
  ]
    where
      myRecompileCmd =
        "xmonad --recompile && (killall xmobar; xmonad --restart)"

-- from altercation/dotfiles-tilingwm
nextNonEmptyWS = findWorkspace getSortByIndexNoSP Next HiddenNonEmptyWS 1
        >>= \t -> (windows . W.view $ t)
prevNonEmptyWS = findWorkspace getSortByIndexNoSP Prev HiddenNonEmptyWS 1
        >>= \t -> (windows . W.view $ t)
getSortByIndexNoSP =
        fmap (.namedScratchpadFilterOutWorkspace) getSortByIndex
---------------------------------------------
-- scratchpad用の設定
---------------------------------------------

mySPConf =
  [ NS "sp01"
       "kitty -T SCPad" 
       (title =? "SCPad")
       (customFloating $ W.RationalRect 0.5 0.05 0.48 0.5) 
  , NS "sp02"
       "kitty -T ranger ranger" 
       (title =? "ranger") 
       (customFloating $ W.RationalRect 0.05 0.1 0.9 0.8) 
  ]


---------------------------------------------
-- プロンプト用PP
---------------------------------------------
myXPConfig = def
    { font    = "xft:M+1 mn:size=14:medium:antialias=true"
    , position = CenteredAt 0.5 0.8 
    , height   = 40
    }

---------------------------------------------
-- DynamicProcect 用のアクション
---------------------------------------------
projects :: [Project]
projects =
  [ Project   "home"    "~/"          (Just $ spawn "kitty")
  , Project   "misc"    "~/"          Nothing
  , Project   "image"   "~/Images"    (Just  $ spawn "geeqie")
  , Project   "config"  "~/.config"   Nothing
  , Project   "code"    "~/code"      (Just $ spawn "kitty")
  , Project   "web"     "~/"          (Just $ spawn "chromium")
  , Project   "rstudio" "~/"          (Just $ spawn "rstudio-bin")
  , Project   "slack" "~/"          (Just $ spawn "slack")
  
  -- Archlinux page for application list 
  , Project   
      "applist" 
      "~/"
      (Just $ spawn $ "chromium " ++ url_applist ++ " --new-window")
  
  -- twitterdeck
  , Project   
      "twitter" 
      "~/"
      (Just $ spawn $ "chromium --app=" ++ url_twitter ++ " --new-window")
  
  -- for xmonad config
  , Project   
      "xmonad" 
      "~/code/xmonad-config"
      (Just $ do spawn "chromium https://xmonad.org/documentation.html --new-window" 
                 spawn "kitty lazygit")
  ]
   where 
      url_applist = "https://wiki.archlinux.org/index.php/List_of_applications"
      url_twitter = "https://tweetdeck.twitter.com/"



