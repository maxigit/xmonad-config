import XMonad hiding((|||))
import qualified Data.Map as M
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Prompt.RunOrRaise
import XMonad.Actions.WindowBringer
import XMonad.Actions.UpdateFocus
import XMonad.Actions.CopyWindow
import XMonad.Actions.Submap

       -- to enable layout jump
import XMonad.Layout.LayoutCombinators -- ((|||), JumpToLayout)
import XMonad.Layout.Renamed (renamed, Rename(Replace))
 
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FadeInactive
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import System.IO
import Data.Bits(complement, (.&.))


layout = name "Hor" tiled
     ||| name "Ver" (Mirror tiled)
     ||| name "Full"Full
     ||| name "HorG" tiledG
     ||| name "VerG" (Mirror tiledG)
  where
    name n = renamed [Replace n]
    tiled = Tall 1 (10/100) (1/2)
    tiledG = Tall 1 (10/100) (g/(1+g))
    g= 1.61 -- Golden ratio
       
main = do
    xmproc <- spawnPipe "xmobar"

    let config =  defaultConfig
                       { manageHook = manageDocks <+> manageHook defaultConfig
                       , startupHook = adjustEventInput
                       , handleEventHook = focusOnMouseMove
                       , layoutHook = avoidStruts layout
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
                       ( [ ("g", gotoMenu)
                         , ("b", bringMenu)
                         , ("M1-S-<Space>", runOrRaisePrompt defaultXPConfig)
                         , (";", xmonadPrompt defaultXPConfig)
                         , ("f", sendMessage $ JumpToLayout "Full")
                         , ("h", sendMessage $ JumpToLayout "Hor")
                         , ("v", sendMessage $ JumpToLayout "Ver")
                         , ("S-h", sendMessage $ JumpToLayout "HorG")
                         , ("S-v", sendMessage $ JumpToLayout "VerG")
                         ] 
                       ++ [("c " ++ show i, windows $ copy (show i)) | i <- [1..9]]
                       )

        modm = mod4Mask
        confKeys = keys config
    xmonad $ config { keys = remap (mod1Mask, xK_space) confKeys  }

remap mod keys k = let keys' k = M.mapKeys resetModifier (keys k)
                       resetModifier (m, k) = (m .&. complement mod4Mask, k)
                 in M.fromList [(mod, submap $ keys' k)]
          
