import XMonad
import XMonad.Config.Xfce
import qualified Data.Map as M
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Actions.WindowBringer

main = xmonad $ defaultConfig { keys = myKeys } where
  myKeys (conf@XConfig {XMonad.modMask = modm}) = M.fromList
         [((modm, xK_g ), gotoMenu)
         ,((modm, xK_b ), bringMenu)
         ]


