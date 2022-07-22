import XMonad
import XMonad.Config.Kde
import qualified XMonad.StackSet as W -- to shift and float windows
import qualified Data.Map as M
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import System.IO
import System.Directory
import Graphics.X11.ExtraTypes.XF86
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.WorkspaceCompare
import qualified XMonad.Actions.FlexibleManipulate as Flex

-- actions
import XMonad.Actions.WindowMenu
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.CycleWS
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import XMonad.Actions.MouseResize

-- hooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.UrgencyHook

-- layout
import XMonad.Layout.Grid
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Tabbed

-- layout modifiers
import XMonad.Layout.Spacing
import XMonad.Layout.WindowArranger
import XMonad.Layout.PositionStoreFloat
import XMonad.Layout.BorderResize
import XMonad.Layout.WindowArranger
import XMonad.Layout.WindowNavigation
import XMonad.Layout.SubLayouts
import XMonad.Layout.TrackFloating
import XMonad.Layout.Fullscreen (fullscreenFull, fullscreenManageHook, fullscreenSupport)
import XMonad.Layout.NoBorders
import XMonad.Layout.LayoutModifier
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed

-- start config

myModMask     = mod4Mask -- use the Windows key as mod
myBorderWidth = 3        -- set window border size
myTerminal    = "alacritty" -- preferred terminal emulator
myFocusBorderColor = "#ebdbb2"
myNormalBorderColor = "#282828"
myFont :: String
myFont = "xft:SauceCodePro Nerd Font Mono:regular:size=9:antialias=true:hinting=true"

-- -------- --
-- KEYBINDS --
-- -------- --
myKeys = 
  -- resize windows with shift + arrow keys
  [ ("M-C-<R>", sendMessage Expand)
  , ("M-C-<L>", sendMessage Shrink)
  , ("M-C-<U>", sendMessage MirrorExpand)
  , ("M-C-<D>", sendMessage MirrorShrink)
  -- change window focus using arrow keys
  -- needs import XMonad.Layout.WindowNavigation
  , ("M-<R>", sendMessage $ Go R)
  , ("M-<L>", sendMessage $ Go L)
  , ("M-<U>", sendMessage $ Go U)
  , ("M-<D>", sendMessage $ Go D)
  -- swap windows with arrow keys
  , ("M-S-<R>", sendMessage $ Swap R)
  , ("M-S-<L>", sendMessage $ Swap L)
  , ("M-S-<U>", sendMessage $ Swap U)
  , ("M-S-<D>", sendMessage $ Swap D)
  , ("M-S-f", withFocused toggleFloat               ) --Toggle focused window floating/tiled
  , ("M-S-m", withFocused centerWindow              ) --Center focused floating window
  , ("M-o", spawn "flameshot gui&")
   -- Run xmessage with a summary of the default keybindings (useful for beginners)
  , ("M-S-/", spawn ("$HOME/.xmonad/rofi_help.sh"))
  , ("M-S-p", spawn ("systemctl --user restart picom")) -- restart picom
  ]
  where
        toggleFloat w = windows (\s -> if M.member w (W.floating s)
                        then W.sink w s
                        else W.float w (W.RationalRect (1/3) (1/4) (1/2) (4/5)) s)

myKeysToRemove = 
  [ -- ("M-p") -- remove default mod + p keybind
  ]
-- ------------- --
-- startup stuff --
-- ------------- --
myStartupHook :: X ()
myStartupHook = do
  spawn "killall latte-dock"
  spawn "killall xembedsniproxy"
  spawn "$HOME/.config/polybar/launch.sh"
  spawn "dunst --config $HOME/.config/dunst/dunstrc&"

--Layout settings
centerWindow :: Window -> X ()
centerWindow win = do
    (_, W.RationalRect x y w h) <- floatLocation win
    windows $ W.float win (W.RationalRect ((1 - w) / 2) ((1 - h) / 2) w h)
    return ()

myManageHook :: ManageHook
myManageHook = manageDocks <+> coreManageHook

coreManageHook :: ManageHook
coreManageHook = composeAll . concat $
  [ [ className   =? c --> doCenterFloat | c <- myFloats]
  , [ isDialog         --> doCenterFloat]
  , [ title       =? t --> doFloat | t <- myOtherFloats]
  , [ (className =? "firefox" <&&> resource =? "Dialog") --> doFloat]  -- Float Firefox Dialog
  , [ className   =? c --> doF (W.shift "2") | c <- webApps]
  , [ className   =? c --> doF (W.shift "3") | c <- devApps]
  , [ className   =? "lattedock" --> doIgnore]
  , [ isFullscreen -->  doFullFloat]
  ]
  where myFloats = 
          [ "MPlayer"
          , "Gimp"
          , "Plasma-desktop"
          , "plasmashell"
          , "krunner"
          , "Alacritty"
          , "transmission"
          , "dolphin"
          , "kate"
          , "systemsettings"
          , "heroic"
          , "Lutris"
          , "Steam"
          , "Spotify"
          , "gwenview"
          , "stacer"
          , "Bottles"
          , "wechat.exe"
          ]
        myOtherFloats = 
          [ "alsamixer"
          , "notification"
          ]
        webApps       = ["firefox", "librewolf", "Google-chrome"] -- open on desktop 2
        devApps       = ["VSCodium"] -- open on desktop 3

myLayoutHook = mouseResize $ windowArrange $ windowNavigation $ avoidStruts $ borderResize $ fullscreenFull coreLayoutHook 

tabs     = renamed [Replace "tabs"]
           -- I cannot add spacing to this layout because it will
           -- add spacing between window and tabs which looks bad.
           $ smartSpacing 0
           $ tabbed shrinkText myTabTheme

myTabTheme = def { fontName            = myFont
                 , activeColor         = myFocusBorderColor
                 , inactiveColor       = myNormalBorderColor
                 , activeBorderColor   = myFocusBorderColor
                 , inactiveBorderColor = myNormalBorderColor
                 , activeTextColor     = myNormalBorderColor
                 , inactiveTextColor   = myFocusBorderColor
                 }

coreLayoutHook = tiled ||| Mirror tiled ||| noBorders Full ||| spacing 8 tiled3 ||| spacing 8 Grid ||| simplestFloat ||| tabs
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = spacing 8 $ ResizableTall nmaster delta ratio []
    -- like the tall, but with three columns
    tiled3  = ThreeColMid nmaster delta ratio
    -- The default number of windows in the master pane
    nmaster = 1
    -- Percent of screen to increment by when resizing panes
    delta   = 3/100
    -- Default proportion of screen occupied by master pane
    ratio   = 1/2
   

main :: IO ()
main = xmonad $ ewmhFullscreen $ ewmh $ docks $ withUrgencyHook NoUrgencyHook $ kdeConfig
    { modMask = myModMask -- use the Windows button as mod
    , borderWidth = myBorderWidth
    , manageHook = manageHook kdeConfig <+> myManageHook
    , layoutHook = myLayoutHook
    , terminal = myTerminal
    , normalBorderColor = myNormalBorderColor
    , focusedBorderColor = myFocusBorderColor
    , handleEventHook = handleEventHook def
    , startupHook = myStartupHook
    } 
    -- `removeKeysP` myKeysToRemove
    `additionalKeysP` myKeys
