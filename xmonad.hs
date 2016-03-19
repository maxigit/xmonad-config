import XMonad hiding((|||))
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import System.Exit
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Prompt.RunOrRaise
import XMonad.Actions.WindowBringer
import XMonad.Actions.Commands
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
import XMonad.Util.EZConfig(additionalKeysP, mkNamedKeymap)
import XMonad.Util.NamedActions
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
        -- list of command and keymap. the command name will be used as a name (for documentation)
        -- as well as identifier to be ran via dmenu
        -- @ will be replace by the "leader"
        commands = [
                   -- layout
                     ("@f", "Full" , sendMessage $ JumpToLayout "Full")
                   , ("@h", "Horizontal", sendMessage $ JumpToLayout "Hor")
                   , ("@S-h", "Horizontal Golden", sendMessage $ JumpToLayout "HorG")
                   , ("@v", "Vertical", sendMessage $ JumpToLayout "Ver")
                   , ("@S-v", "Vertical Golden", sendMessage $ JumpToLayout "VerG")
                   -- global
                   , ("@q S-q", "Quit XMonad", io (exitWith ExitSuccess))
                   , ("@q q", "Restart XMonad", spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
                   , ("@ <Space>", "Prompt", runCommand commands')
                   , ("@ r", "Prompt", runCommand commands')
                   , ("M1-;", "run or raise", runOrRaisePrompt defaultXPConfig)
                   -- windows
                     , ("@<", "Shrink", sendMessage Shrink)
                     , ("@>", "Shrink", sendMessage Expand)
                     , ("@,", "Decrement master", sendMessage $ IncMasterN (-1) ) -- 
                     , ("@.", "Increment master", sendMessage $ IncMasterN 1)
                   --   focus
                     , ("@m", "Focus Master", windows W.focusMaster)
                     , ("@n", "Focus Next", windows W.focusDown)
                     , ("@S-n", "Swap Next", windows W.swapUp)
                     , ("@S-m", "Swap Master", windows W.swapMaster)
                   --   layerk
                   -- applications
                       , ("@ a t", "teriminal", spawn =<< asks (terminal . XMonad.config))
                   -- workspaces
                   ]
        commands' = [(s, c) | (_,s,c) <- commands, s /= ""]
        myKeys c = (subtitle "Custom Keys": ) $ mkNamedKeymap c [(processKey key, addName name command) | (key, name, command) <- commands, key /= ""]
        -- (subtitle "Custom Keys":) $ mkNamedKeymap c $
        --                [ ("M1-S-;", addName "run command" $ runCommand commands') ]
        processKey ('@':k) = "M1-<Space> " ++ k
        processKey k = k

        modm = mod4Mask
        confKeys = keys config
    -- xmonad $ config -- { keys = remap (mod1Mask, xK_space) confKeys  }
    xmonad $ addDescrKeys ((modm, xK_F1), xMessage) myKeys config


remap mod keys k = let keys' k = M.mapKeys resetModifier (keys k)
                       resetModifier (m, k) = (m .&. complement mod4Mask, k) -- 
                 in M.fromList [(mod, submap $ keys' k)]
          
