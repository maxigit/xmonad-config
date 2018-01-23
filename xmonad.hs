import XMonad hiding((|||))
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import System.Exit
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Prompt.RunOrRaise
import XMonad.Actions.WindowGo
import XMonad.Actions.WindowBringer
import XMonad.Actions.Commands
import XMonad.Actions.UpdateFocus
import XMonad.Actions.CopyWindow
import XMonad.Actions.Submap
import XMonad.Actions.CycleWS
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Actions.FocusNth
import XMonad.Actions.Promote
import XMonad.Actions.Search
import XMonad.Actions.RotSlaves

       -- to enable layout jump
import XMonad.Layout.LayoutCombinators -- ((|||), JumpToLayout)
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.TwoPane
import XMonad.Layout.Grid
import XMonad.Layout.LimitWindows
 
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FadeInactive
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP, mkNamedKeymap, mkKeymap)
import XMonad.Util.NamedActions
import System.IO
import Data.Bits(complement, (.&.))
import Data.Char (toLower)


-- layout = toggleLayouts Full layout'
layout = toggleLayouts Full $ limitSlice 6 layout'
layout' = name "Hor" tiled
     ||| name "Ver" (Mirror tiled)
     -- ||| name "Full" Full
     ||| name "HorG" tiledG
     ||| name "VerG" (Mirror tiledG)
     ||| name "Grid"  Grid
     -- ||| name "Hor2" twoP
     -- ||| name "Ver2" (Mirror twoP)
  where
    name n = renamed [Replace n]
    tiled = Tall 1 (10/100) (1/2)
    tiledG = Tall 1 (10/100) (g/(1+g))
    g= 1.61 -- Golden ratio
    twoP = TwoPane (3/100) (1/2)
       
extraWs = "abcdfghijkmorstuvxyz"

myXmobarHook xmproc =  do
  -- workspace containing the focused window
  copies <- wsContainingCopies
  -- print in red workspace containing a copy of the focuse window
  let checkTag ws | ws `elem` copies = pad . xmobarColor "red" "black" $ ws 
                  | otherwise = pad ws
  dynamicLogWithPP xmobarPP
        { ppOutput = hPutStrLn xmproc
        , ppTitle = xmobarColor "green" "" . shorten 50
        , ppHidden = checkTag
        }
main = do
    xmproc <- spawnPipe "xmobar"

    let config =  defaultConfig
                       { manageHook = manageDocks <+> manageHook defaultConfig
                       , startupHook = adjustEventInput
                       , handleEventHook = focusOnMouseMove
                       , layoutHook = avoidStruts layout
                       , logHook = myXmobarHook xmproc <+> fadeInactiveLogHook 0.8
                       , modMask = modm     -- Rebind Mod to the Windows key
	               , borderWidth = 1
	               , focusedBorderColor = "#ff0000" -- "#ffffff"
	               , normalBorderColor = "#000000"
                       , workspaces = map show [1..9] ++ map return extraWs
                       } `additionalKeysP`
                       ( [ ("g", gotoMenu)
                         , ("b", bringMenu)
                         , ("M1-S-<Space>", runOrRaisePrompt xpConfig)
                         , (";", xmonadPrompt xpConfig)
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
                     ("@f", "Full" , sendMessage $ ToggleLayout)
                   , ("@S-f", "Grid" , sendMessage $ JumpToLayout "Grid")
                   , ("@h", "Horizontal", sendMessage $ JumpToLayout "Hor")
                   , ("@S-h", "Horizontal Golden", sendMessage $ JumpToLayout "HorG")
                   , ("@v", "Vertical", sendMessage $ JumpToLayout "Ver")
                   , ("@S-v", "Vertical Golden", sendMessage $ JumpToLayout "VerG")
		   -- , ("@2", "Two Pane Layout", sendMessage $ JumpToLayout "Hor2")
		   , ("@1", "Full Screen", setLimit 1)
		   , ("@2", "Two Panes Limit", setLimit 2)
		   , ("@3", "Three Panes Layout", setLimit 3)
		   , ("@6", "Two Pane Layout", setLimit 6)
		   , ("@4", "Decrease limit", decreaseLimit)
		   , ("@7", "Increase limit", increaseLimit)
		   -- , ("@S-2", "Two Pane Vertical", sendMessage $ JumpToLayout "Ver2")
                   -- global
                   , ("@q S-q", "Quit XMonad", io (exitWith ExitSuccess))
                   , ("@q q", "Restart XMonad", spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
                   , ("@q t", "Toggle Struts", sendMessage ToggleStruts ) -- workaround xmobar not showing on screen1
                   , ("@l r", "Reset layouts", setLayout =<< asks (XMonad.layoutHook  . XMonad.config ))
                   , ("@ <Space>", "Prompt", runCommand commands')
                   , ("@ r", "Prompt", xmonadPromptC commands' xpConfig)
                   , ("@S-r", "Refresh", refresh)
                   , ("M1-;", "run or raise", runOrRaisePrompt xpConfig)
                   , ("@ a a", "run or raise", runOrRaisePrompt xpConfig)
                   -- windows
                     , ("@S-,", "Shrink", sendMessage Shrink)
                     , ("@S-.", "Expand", sendMessage Expand)
                     , ("@,", "Decrement master", sendMessage $ IncMasterN (-1) ) -- 
                     , ("@.", "Increment master", sendMessage $ IncMasterN 1)
                     , ("@S-s", "Sink window", withFocused $ windows . W.sink)
                     , ("@ g", "Goto window", gotoMenu )
                     , ("@ b", "Goto window", bringMenu )
                     , ("@ d", "Delete window", kill1 )
                     , ("@S-d", "Delete all copy window", killAllOtherCopies )
                   --   focus
                     , ("@m", "Focus Master", windows W.focusMaster)
                     , ("@n", "Focus Next", windows W.focusDown)
                     , ("@e", "Focus Previous", rotSlavesDown)
                     , ("@S-e", "Swap Previous", rotSlavesUp)
                     , ("@S-n", "Swap next", windows W.swapUp)
                     , ("@S-m", "Swap master", windows W.swapMaster)
                     , ("@w w", "Focus Next", promote) -- windows W.focusDown)
                   -- applications
                       , ("@ a t", "terminal", spawn "gnome-terminal") -- =<< asks (terminal . XMonad.config))
                       , ("@ a f", "Firefox", spawn "firefox")
                       , ("@ a e", "Emacs", spawn "emacs")
                       , ("@ a E", "Emacs -nw", spawn "emacs")
                       , ("@ a n", "nautilus", spawn "nautilus")
                   -- Search
                       , ("@ s g", "Search in Dictionary", promptSearch' xpConfig google)
                       , ("@ s h", "Search in Dictionary", promptSearch' xpConfig hoogle)
                       , ("@ s k", "Search in Dictionary", promptSearch' xpConfig hackage)
                       , ("@ s w", "Search in Dictionary", promptSearch' xpConfig wikipedia)
                       , ("@ s s", "Search in Dictionary", promptSearch' xpConfig multi)
                   -- workspaces
                       , ("@ l l", "Toggle to previous Workspace ", toggleWS)
                       , ("@ l n", "Switch to next (non-empty) workspace ", moveTo Next NonEmptyWS  )
                       , ("@ l S-n", "Switch to next Workspace ", nextWS)
                       , ("@ l e", "Switch to next empty workspace ", moveTo Next EmptyWS  )
                       , ("@ l p", "Switch to previous (non-empty) workspace ", moveTo Prev NonEmptyWS  )
                       , ("@ l S-p", "Switch to previous Workspace ", prevWS )
                       , ("@ p e", "Push and go to empty workspace", tagToEmptyWorkspace)
                       , ("@ S-p e", "Push to empty workspace", sendToEmptyWorkspace)
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
		   ++ -- Workspaces operations
                   [ (key ++ [c], description ++ [c], sequence_ $ map windows (map ($ [c]) command))
                   | c <- extraWs :: [Char]
		   , (key, description, command) <- [ -- ("M1-",  "Switch to ", [W.greedyView])
					             ("@ l ", "Layer ", [W.greedyView])
					            -- , ("M1-S-", "Shift (push) ", [W.shift])
					            , ("@ S-p ", "Push ", [W.shift])
			 		            , ("@ p ",  "Push and go ", [W.shift, W.greedyView])
			 	                    , ("@ S-t ", "Put ", [copy])
			 		            , ("@ t ", "Put and go ", [copy, W.greedyView])
			 		            , ("@ t ", "Put and go ", [copy, W.greedyView])
-- d delete
-- D delete all others
			 		           ]
	           ]
                  ++ -- windows operations
                  [ ("@ w " ++ show i, "Focus to " ++ show i, focusNth (i-1) )
                    | i <- [1..9]
                  ]
                  ++
                  -- screens
                  [ (key ++ [sk], description ++ show sc, screenWorkspace sc >>= flip whenJust (windows . command)  )
                  | (sk, sc)   <- zip "qwf" [1..]
                  , (key, description, command) <- [("@ l ", "Swith to screen ", W.view)
                                                   ,("@ p ", "Push to screen ", W.shift)
                                                   ]
                  ]
        commands' = [(s, c) | (_,s,c) <- commands, s /= ""]
	-- commands' = [("dummy", return ())]
        myKeysWithName c = (subtitle "Custom Keys": ) $ mkNamedKeymap c [(processKey key, addName name command) | (key, name, command) <- commands, key /= ""]
        myKeys c = mkKeymap c [(processKey key, command) | (key, name, command) <- commands, key /= ""]
        -- (subtitle "Custom Keys":) $ mkNamedKeymap c $
        --                [ ("M1-S-;", addName "run command" $ runCommand commands') ]
    
        -- There
        -- processKey ('@':k) = "M1-<Space> " ++ processKey k
        processKey ('@':k) = "C-<Space> " ++ processKey k

        processKey k = k

        modm = mod4Mask
        confKeys = keys config
    -- xmonad $ config -- { keys = remap (mod1Mask, xK_space) confKeys  }
    xmonad $ config { keys = myKeys}


remap mod keys k = let keys' k = M.mapKeys resetModifier (keys k)
                       resetModifier (m, k) = (m .&. complement mod4Mask, k) -- 
                 in M.fromList [(mod, submap $ keys' k)]
          

xpConfig = defaultXPConfig { position = Top
                           , font =         "xft:Bitstream Vera Sans Mono:size=12:bold:antialias=true"
                           , searchPredicate = ignoreCase
                           }
  where ignoreCase p s = searchPredicate defaultXPConfig (ic p) (ic s)
        ic = map toLower
promptSearch' config engine = promptSearch config engine >> raise (className =? "Firefox" <||> className =? "Firefox-bin")
