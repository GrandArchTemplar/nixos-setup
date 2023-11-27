import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

main :: IO()
main = xmonad =<< xmobar myConfig 

myConfig = def 
  { modMask = mod4Mask 
  , terminal = "alacritty"
  , workspaces = myWorkspaces
  } `additionalKeys` myKeys

myWorkspaces = ["1:main", "2:web", "3:tg", "4", "5", "6", "7", "8", "9"]

myKeys = 
  [ ((mod1Mask , xK_Shift_L          ), spawn keyboard_switch)
  , ((shiftMask, xK_Alt_L            ), spawn keyboard_switch)
  , ((controlMask .|. mod4Mask , xK_l), spawn lockscreen)
  ]

lockscreen = "i3lock-color -c 181818 -e -k --time-color=cfcfcf --date-color=cfcfcf"
keyboard_switch = "~/.config/xmonad/keyboard_switch.sh"
