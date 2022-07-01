import XMonad
import XMonad.Config.Kde
import qualified XMonad.StackSet as W -- to shift and float windows
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import System.IO
import System.Directory
import Graphics.X11.ExtraTypes.XF86
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.WorkspaceCompare

-- actions
import XMonad.Actions.WindowMenu
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.CycleWS
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO

-- hooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops


-- layout
import XMonad.Layout.Grid
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ImageButtonDecoration
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.WindowArranger
import XMonad.Layout.PositionStoreFloat
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.BorderResize
import XMonad.Layout.WindowArranger
import XMonad.Layout.WindowNavigation

-- start config

myModMask     = mod4Mask -- use the Windows key as mod
myBorderWidth = 2        -- set window border size
myTerminal    = "alacritty" -- preferred terminal emulator
myFocusBorderColor = "#89b4fa"
myNormalBorderColor = "#313244"

--
--IN_PROGRESS: keybinds
--
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
  ]

myManageHook :: ManageHook
myManageHook = manageDocks <+> coreManageHook

coreManageHook :: ManageHook
coreManageHook = composeAll . concat $
  [ [ className   =? c --> doFloat           | c <- myFloats]
  , [ title       =? t --> doFloat           | t <- myOtherFloats]
  , [ className   =? c --> doF (W.shift "2") | c <- webApps]
  , [ className   =? c --> doF (W.shift "3") | c <- ircApps]
  , [ isDialog         --> doFloat]
  , [ className   =? "lattedock" --> doIgnore]
  ]
  where myFloats = 
          [ "MPlayer"
          , "Gimp"
          , "Plasma-desktop"
          , "plasmashell"
          , "krunner"
          ]
        myOtherFloats = ["alsamixer"]
        webApps       = ["Firefox-bin", "librewolf", "Google-chrome"] -- open on desktop 2
        ircApps       = ["Ksirc"]                -- open on desktop 3

myLayoutHook = windowNavigation $ spacing 10 $ avoidStruts $ borderResize $ coreLayoutHook 

coreLayoutHook = tiled ||| Mirror tiled ||| Full ||| tiled3 ||| Grid
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = ResizableTall nmaster delta ratio []
    -- like the tall, but with three columns
    tiled3  = ThreeColMid nmaster delta ratio
    -- The default number of windows in the master pane
    nmaster = 1
    -- Percent of screen to increment by when resizing panes
    delta   = 3/100
    -- Default proportion of screen occupied by master pane
    ratio   = 1/2

main :: IO ()
main = xmonad $ kdeConfig
    { modMask = myModMask -- use the Windows button as mod
    , borderWidth = myBorderWidth
    , manageHook = manageHook kdeConfig <+> myManageHook
    , layoutHook = myLayoutHook
    , terminal = myTerminal
    , normalBorderColor = myNormalBorderColor
    , focusedBorderColor = myFocusBorderColor
    , handleEventHook = handleEventHook def <+> fullscreenEventHook <+> docksEventHook
    } 
    `additionalKeysP` myKeys
