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

-- hooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops

-- layout
import XMonad.Layout.Grid
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns
import XMonad.Layout.SimplestFloat

-- layout modifiers
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.WindowArranger
import XMonad.Layout.PositionStoreFloat
import XMonad.Layout.BorderResize
import XMonad.Layout.WindowArranger
import XMonad.Layout.WindowNavigation
import XMonad.Layout.SubLayouts
import XMonad.Layout.TrackFloating

-- start config

myModMask     = mod4Mask -- use the Windows key as mod
myBorderWidth = 3        -- set window border size
myTerminal    = "alacritty" -- preferred terminal emulator
myFocusBorderColor = "#ebdbb2"
myNormalBorderColor = "#928374"

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
  , ("M-S-/", spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
  , ("M-p", spawn "systemctl --user restart picom") -- restart picom
  ]
  where
        toggleFloat w = windows (\s -> if M.member w (W.floating s)
                        then W.sink w s
                        else W.float w (W.RationalRect (1/3) (1/4) (1/2) (4/5)) s)

myKeysToRemove = 
  [ ("M-p") -- remove default mod + p keybind
  ]
-- ------------- --
-- startup stuff --
-- ------------- --
myStartupHook :: X ()
myStartupHook = do
    spawn "killall -q latte-dock; latte-dock --layout \"Moe - 2\" &"
    -- spawn "systemctl --user restart picom "

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
    -- , handleEventHook = handleEventHook def <+> fullscreenEventHook <+> docksEventHook
    , startupHook = myStartupHook
    } 
    `additionalKeysP` myKeys
    `removeKeysP` myKeysToRemove

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
