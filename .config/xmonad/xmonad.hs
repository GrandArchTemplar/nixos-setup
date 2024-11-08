import Data.Map qualified as M
import System.Exit
import System.IO
import Text.XHtml (base)
import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.IndependentScreens
import XMonad.Layout.PerWorkspace
import XMonad.ManageHook (composeAll)
import XMonad.StackSet qualified as W
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Loggers
import XMonad.Util.Run (spawnPipe)

main :: IO ()
main = master

master :: IO ()
master = xmonad . ewmhFullscreen =<< xmobar0 =<< xmobar1 myConfig

xmobarCfg n = "~/.config/xmonad/xmobarrc" ++ show n ++ ".hs" 
xmobarBin = "xmobar"
xmobarParam = "-x"
xmobarScreenNumber = show

myCrunchPP :: PP -> PP
myCrunchPP pp =
  pp
    { ppRename = ppRename pp . masterTransmute . telegramTransmute . webTransmute
    }
 where
  masterTransmute s = if s == masterWorkspace then "1_master" else s
  telegramTransmute s = if s == telegramWorkspace then "1_tg" else s
  webTransmute = flip if' "0_web" =<< (webWorkspace ==)
  if' bool t e = if bool then t else e

myXmobarPP :: PP
myXmobarPP =
  xmobarPP
    { ppSep = magenta " • "
    , ppTitleSanitize = xmobarStrip
    , ppCurrent = wrap " " "" . xmobarBorder "Top" "#1eff1e" 2
    , ppHidden = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent = red . wrap (yellow "!") (yellow "!")
    , ppOrder = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras = [logTitles formatFocused formatUnfocused]
    }
 where
  formatFocused = wrap (white "[") (white "]") . magenta . ppWindow
  formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue . ppWindow

  -- \| Windows should have *some* title, which should not not exceed a
  -- sane length.
  ppWindow :: String -> String
  ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

  blue, lowWhite, magenta, red, white, yellow :: String -> String
  magenta = xmobarColor "#ff79c6" ""
  blue = xmobarColor "#bd93f9" ""
  white = xmobarColor "#f8f8f2" ""
  yellow = xmobarColor "#f1fa8c" ""
  red = xmobarColor "#ff5555" ""
  lowWhite = xmobarColor "#a0a0b0" ""
  green = xmobarColor "#1eff1e" ""

xmobar0 = statusBar (unwords [xmobarBin, xmobarCfg 0, xmobarParam, xmobarScreenNumber 0]) (myCrunchPP . marshallPP (S 0) $ myXmobarPP) toggleStrutsKey
xmobar1 = statusBar (unwords [xmobarBin, xmobarCfg 1, xmobarParam, xmobarScreenNumber 1]) (myCrunchPP . marshallPP (S 1) $ myXmobarPP) toggleStrutsKey

toggleStrutsKey XConfig{XMonad.modMask = modMask} = (modMask, xK_b)

myConfig =
  def
    { modMask = myModMask
    , terminal = myTerminal
    , workspaces = myWorkspaces
    , normalBorderColor = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , handleExtraArgs = myHandleExtraArgs
    , layoutHook = myLayout
    , manageHook = myManageHook
    , startupHook = myStartupHook
    , keys = myKeys
    }
    `additionalKeys` myAddKeys

-- myWorkspaces = withScreens 2 (map show [1, 2, 3, 4, 5])
masterWorkspace = "1_1"
webWorkspace = "0_1"
telegramWorkspace = "1_2"
myWorkspaces = [webWorkspace, masterWorkspace, "0_2", telegramWorkspace, "0_3", "1_3", "0_4", "1_4", "0_5", "1_5"]

myHandleExtraArgs xs theConf = case xs of
  [] -> return theConf
  _ -> fail ("unrecognized flags:" ++ show xs)

myManageHook :: ManageHook
myManageHook =
  composeAll
    [ className =? "Code" --> doShift masterWorkspace
    , className =? "Google-chrome" --> doShift webWorkspace
    , className =? "TelegramDesktop" --> doShift telegramWorkspace
    ]

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "alacritty"

myNormalBorderColor :: String
myNormalBorderColor = "gray"

myFocusedBorderColor :: String
myFocusedBorderColor = "green"

myAddKeys =
  [ ((mod1Mask, xK_Shift_L), spawn keyboardSwitch)
  , ((shiftMask, xK_Alt_L), spawn keyboardSwitch)
  , ((controlMask .|. myModMask, xK_l), spawn lockscreen)
  , ((myModMask .|. shiftMask, xK_s), spawn screenshot)
  ]

defaultLayout = tiled ||| Mirror tiled ||| Full
 where
  tiled = Tall nmaster delta ratio
  nmaster = 1
  ratio = 1 / 2
  delta = 3 / 100

myLayout = onWorkspace masterWorkspace Full . onWorkspace telegramWorkspace Full $ defaultLayout

myStartupHook = do
  spawn wallpaper
  spawn noblackscreen
  spawn cursor

lockscreen = "i3lock-color -c 181818 -e -k --time-color=cfcfcf --layout-color=cfcfcf --date-color=cfcfcf --date-str=\"%A: %d/%m/%Y\" --keylayout 0 --radius 120"
keyboardSwitch = "~/.config/xmonad/keyboard_switch.sh"
screenshot = "maim -s | xclip -selection clipboard -t image/png"
wallpaper = "feh --bg-fill /home/archgt/.background-image"
noblackscreen = "xset s off -dpms"
cursor = "xsetroot -cursor_name left_ptr"

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig{XMonad.modMask = modMask}) =
  M.fromList $
    -- launching and killing programs
    -- launching and killing programs
    -- launching and killing programs
    -- launching and killing programs

    -- launching and killing programs
    [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf) -- %! Launch terminal
    , ((modMask, xK_p), spawn "dmenu_run") -- %! Launch dmenu
    , ((modMask .|. shiftMask, xK_c), kill) -- %! Close the focused window
    , ((modMask, xK_space), sendMessage NextLayout) -- %! Rotate through the available layout algorithms
    , ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf) -- %!  Reset the layouts on the current workspace to default
    , ((modMask, xK_n), refresh) -- %! Resize viewed windows to the correct size
    , -- move focus up or down the window stack
      ((modMask, xK_Tab), windows W.focusDown) -- %! Move focus to the next window
    , ((modMask .|. shiftMask, xK_Tab), windows W.focusUp) -- %! Move focus to the previous window
    , ((modMask, xK_j), windows W.focusDown) -- %! Move focus to the next window
    , ((modMask, xK_k), windows W.focusUp) -- %! Move focus to the previous window
    , ((modMask, xK_m), windows W.focusMaster) -- %! Move focus to the master window
    , -- modifying the window order
      ((modMask, xK_Return), windows W.swapMaster) -- %! Swap the focused window and the master window
    , ((modMask .|. shiftMask, xK_j), windows W.swapDown) -- %! Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_k), windows W.swapUp) -- %! Swap the focused window with the previous window
    , -- resizing the master/slave ratio
      ((modMask, xK_h), sendMessage Shrink) -- %! Shrink the master area
    , ((modMask, xK_l), sendMessage Expand) -- %! Expand the master area
    , -- floating layer support
      ((modMask, xK_t), withFocused $ windows . W.sink) -- %! Push window back into tiling
    , -- increase or decrease number of windows in the master area
      ((modMask, xK_comma), sendMessage (IncMasterN 1)) -- %! Increment the number of windows in the master area
    , ((modMask, xK_period), sendMessage (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area
    , -- quit, or restart
      ((modMask .|. shiftMask, xK_q), io exitSuccess) -- %! Quit xmonad
    , ((modMask, xK_q), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi") -- %! Restart xmonad
    ]
      ++
      -- mod-[1..5] %! Switch to workspace N
      -- mod-[1..5] %! Switch to workspace N
      -- mod-[1..5] %! Switch to workspace N
      -- mod-[1..5] %! Switch to workspace N
      -- mod-shift-[1..5] %! Move client to workspace N
      -- mod-shift-[1..5] %! Move client to workspace N
      -- mod-shift-[1..5] %! Move client to workspace N
      -- mod-shift-[1..5] %! Move client to workspace N

      -- mod-[1..5] %! Switch to workspace N
      -- mod-shift-[1..5] %! Move client to workspace N
      independentScreenKeys conf
      ++
      -- mod-{w,e} %! Switch to physical/Xinerama screens 1, 2, or 3
      -- mod-{w,e} %! Switch to physical/Xinerama screens 1, 2, or 3
      -- mod-{w,e} %! Switch to physical/Xinerama screens 1, 2, or 3
      -- mod-shift-{w,e} %! Move client to screen 1, 2, or 3
      -- mod-shift-{w,e} %! Move client to screen 1, 2, or 3
      -- mod-shift-{w,e} %! Move client to screen 1, 2, or 3
      [ ((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_w, xK_e] [0 ..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
      ]

onCurrentScreenX :: (PhysicalWorkspace -> X a) -> (VirtualWorkspace -> X a)
onCurrentScreenX f vwsp =
  withCurrentScreen (f . flip marshall vwsp)

withCurrentScreen :: (ScreenId -> X a) -> X a
withCurrentScreen f =
  withWindowSet (f . W.screen . W.current)

independentScreenKeys conf@(XConfig{XMonad.modMask = modMask}) =
  [ ( (m .|. modMask, k)
    , windows =<< onCurrentScreenX f i
    )
  | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_5]
  , (f, m) <-
      [ (pure . W.view, noModMask)
      , (pure . W.shift, shiftMask)
      ]
  ]
