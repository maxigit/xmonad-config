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
import XMonad.Actions.CycleWS
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Actions.FocusNth
import XMonad.Actions.Promote

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
                   , ("@ r", "Prompt", xmonadPromptC commands' defaultXPConfig)
                   , ("@ R", "Refresh", refresh)
                   , ("M1-;", "run or raise", runOrRaisePrompt defaultXPConfig)
                   -- windows
                     , ("@<", "Shrink", sendMessage Shrink)
                     , ("@>", "Shrink", sendMessage Expand)
                     , ("@,", "Decrement master", sendMessage $ IncMasterN (-1) ) -- 
                     , ("@.", "Increment master", sendMessage $ IncMasterN 1)
                   , ("@ S", "Sink window", withFocused $ windows . W.sink)
                   --   focus
                     , ("@m", "Focus Master", windows W.focusMaster)
                     , ("@n", "Focus Next", windows W.focusDown)
                     , ("@S-n", "Swap next", windows W.swapUp)
                     , ("@S-m", "Swap master", windows W.swapMaster)
                     , ("@w w", "Focus Next", promote) -- windows W.focusDown)
                   -- applications
                       , ("@ a t", "terminal", spawn =<< asks (terminal . XMonad.config))
                       , ("@ a f", "Firefox", spawn "firefox")
                       , ("@ a e", "Emacs", spawn "emacs")
                       , ("@ a E", "Emacs -nw", spawn "emacs")
                   -- workspaces
                       , ("@ l l", "Toggle to previous Workspace ", toggleWS)
                       , ("@ l n", "Switch to next (non-empty) workspace ", moveTo Next NonEmptyWS  )
                       , ("@ l S-n", "Switch to next Workspace ", nextWS)
                       , ("@ l e", "Switch to next empty workspace ", moveTo Next EmptyWS  )
                       , ("@ l p", "Switch to previous (non-empty) workspace ", moveTo Prev NonEmptyWS  )
                       , ("@ l S-p", "Switch to previous Workspace ", prevWS )
                       , ("@ p e", "Push and go to empty workspace", tagToEmptyWorkspace)
                       , ("@ p S-e", "Push to empty workspace", sendToEmptyWorkspace)
                   ]
		   ++ -- Workspaces operations
                   [ (key ++ show i, description ++ show i, sequence_ $ map windows (map ($show i) command))
                   | i <- [1..9] :: [Int]
		   , (key, description, command) <- [ ("M1-",  "Switch to ", [W.greedyView])
					            , ("@ l ", "Layer ", [W.greedyView])
					            , ("M1-S-", "Shift (push) ", [W.shift])
					            , ("@ S-p ", "Push ", [W.shift])
			 		            , ("@ p ",  "Push and go ", [W.shift, W.greedyView])
			 	                    , ("@ S-t ", "Put ", [copy])
			 		            , ("@ t ", "Put and go ", [copy, W.greedyView])
-- d delete
-- D delete all others
			 		           ]
			 
                  ]
                  ++ -- windows operations
                  [ ("@ w " ++ show i, "Focus to " ++ show i, focusNth (i-1) )
                    | i <- [1..9]
                  ]

        commands' = [(s, c) | (_,s,c) <- commands, s /= ""]
	-- commands' = [("dummy", return ())]
        myKeys c = (subtitle "Custom Keys": ) $ mkNamedKeymap c [(processKey key, addName name command) | (key, name, command) <- commands, key /= ""]
        -- (subtitle "Custom Keys":) $ mkNamedKeymap c $
        --                [ ("M1-S-;", addName "run command" $ runCommand commands') ]
        processKey ('@':k) = "M1-<Space> " ++ processKey k
        processKey k = k

        modm = mod4Mask
        confKeys = keys config
    -- xmonad $ config -- { keys = remap (mod1Mask, xK_space) confKeys  }
    xmonad $ addDescrKeys ((modm, xK_F1), xMessage) myKeys config


remap mod keys k = let keys' k = M.mapKeys resetModifier (keys k)
                       resetModifier (m, k) = (m .&. complement mod4Mask, k) -- 
                 in M.fromList [(mod, submap $ keys' k)]
          
