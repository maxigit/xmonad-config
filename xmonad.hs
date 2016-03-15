import XMonad
import qualified Data.Map as M
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Prompt.RunOrRaise
import XMonad.Actions.WindowBringer
import XMonad.Actions.UpdateFocus
import XMonad.Actions.CopyWindow
 
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import System.IO

main = do
    xmproc <- spawnPipe "xmobar"

    xmonad $ defaultConfig
        { manageHook = manageDocks <+> manageHook defaultConfig
        , startupHook = adjustEventInput
        , handleEventHook = focusOnMouseMove
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , modMask = modm     -- Rebind Mod to the Windows key
	, borderWidth = 2
	-- , focusedBorderColor = "#d00000"
	-- , normalBorderColor = "#000000"
        } `additionalKeysP`
        ( [ ("M1-<Space> g", gotoMenu)
          , ("M1-<Space> b", bringMenu)
          , ("M1-<Space> m", runOrRaisePrompt defaultXPConfig)
          , ("M1-<Space> x", xmonadPrompt defaultXPConfig)

          ] 
        ++ [("M1-<Space> c " ++ show i, windows $ copy (show i)) | i <- [1..9]]
        )

    where modm = mod4Mask
