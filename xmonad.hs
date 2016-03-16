import XMonad
import qualified Data.Map as M
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Prompt.RunOrRaise
import XMonad.Actions.WindowBringer
import XMonad.Actions.UpdateFocus
import XMonad.Actions.CopyWindow
import XMonad.Actions.Submap
 
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FadeInactive
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import System.IO
import Data.Bits(complement, (.&.))

main = do
    xmproc <- spawnPipe "xmobar"

    let config =  defaultConfig
                       { manageHook = manageDocks <+> manageHook defaultConfig
                       , startupHook = adjustEventInput
                       , handleEventHook = focusOnMouseMove
                       , layoutHook = avoidStruts  $  layoutHook defaultConfig
                       , logHook = dynamicLogWithPP xmobarPP
                                       { ppOutput = hPutStrLn xmproc
                                       , ppTitle = xmobarColor "green" "" . shorten 50
                                       }
                                       <+> fadeInactiveLogHook 0.8
                       , modMask = modm     -- Rebind Mod to the Windows key
	               , borderWidth = 1
	               , focusedBorderColor = "#ffffff"
	               , normalBorderColor = "#000000"
                       } `additionalKeysP`
                       ( [ ("M1-<Space> g", gotoMenu)
                         , ("M1-<Space> b", bringMenu)
                         , ("M1-<Space> m", runOrRaisePrompt defaultXPConfig)
                         , ("M1-<Space> ;", xmonadPrompt defaultXPConfig)

                         ] 
                       ++ [("M1-<Space> c " ++ show i, windows $ copy (show i)) | i <- [1..9]]
                       )

        modm = mod4Mask
        confKeys = keys config
    xmonad $ config { keys = remap (mod1Mask, xK_space) confKeys  }

remap mod keys k = let keys' k = M.mapKeys resetModifier (keys k)
                       resetModifier (m, k) = (m .&. complement mod4Mask, k)
                 in M.fromList [(mod, submap $ keys' k)]
          
