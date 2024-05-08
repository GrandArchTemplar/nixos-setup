import System.IO
import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.PerWorkspace
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe)
import Text.XHtml (base)

main :: IO ()
main = xmobar myConfig >>= xmonad

myConfig =
  def
    { modMask = myModMask
    , terminal = myTerminal
    , workspaces = myWorkspaces
    , normalBorderColor = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , handleExtraArgs = myHandleExtraArgs
    , layoutHook = myLayout
    , manageHook = manageSpawn <> manageHook def
    , startupHook = myStartupHook
    }
    `additionalKeys` myKeys

myWorkspaces = ["main", "web", "tg", "4", "5", "6", "7", "8", "9"]

myHandleExtraArgs xs theConf = case xs of
  [] -> return theConf
  _ -> fail ("unrecognized flags:" ++ show xs)

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "alacritty"

myNormalBorderColor :: String
myNormalBorderColor = "gray"

myFocusedBorderColor :: String
myFocusedBorderColor = "green"

myKeys =
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

myLayout = onWorkspace "main" Full . onWorkspace "tg" Full $ defaultLayout

myStartupHook = do
  spawn wallpaper


lockscreen = "i3lock-color -c 181818 -e -k --time-color=cfcfcf --layout-color=cfcfcf --date-color=cfcfcf --date-str=\"%A: %d/%m/%Y\" --keylayout 0 --radius 120"
keyboardSwitch = "~/.config/xmonad/keyboard_switch.sh"
screenshot = "maim -s | xclip -selection clipboard -t image/png"
wallpaper = "feh --bg-fill /home/archgt/.background-image"
