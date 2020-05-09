{-# LANGUAGE OverloadedStrings #-}
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
import XMonad.Actions.WithAll hiding (killAll)

       -- to enable layout jump
import XMonad.Layout.LayoutCombinators -- ((|||), JumpToLayout)
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.TwoPane
import XMonad.Layout.Grid
import XMonad.Layout.Tabbed
-- import XMonad.Layout.Dishes
import XMonad.Layout.Dwindle
import XMonad.Layout.LimitWindows
import XMonad.Layout.NoBorders
 
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FadeInactive
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP, mkNamedKeymap, mkKeymap)
import XMonad.Util.NamedActions
import System.IO
import Data.Bits(complement, (.&.))
import Data.Char (toLower)

import XMonad.Util.Themes
import XMonad.Util.Themes
import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8
-- layout = toggleLayouts Full layout'
-- full = tabbedBottom shrinkText def { activeColor         = "#115422"
--                                       , activeBorderColor   = "#1a8033"
--                                       , activeTextColor     = "white"
--                                       , inactiveColor       = "#543211"
--                                       , inactiveBorderColor = "#804c19"
--                                       , inactiveTextColor   = "#ffcc33"
--                                       , fontName = ""
--                                       }
layout = toggleLayouts (noBorders Full) $ limitSlice 6 layout'
layout' = name "Hor" tiled
     ||| name "Ver" (Mirror tiled)
     -- ||| name "Full" Full
     ||| name "HorG" tiledG
     ||| name "VerG" (Mirror tiledG)
     ||| name "Grid"  Grid
     ||| name "Dwindle" (Dwindle R CW 1.5 1.1)
     -- ||| name "Hor2" twoP
     -- ||| name "Ver2" (Mirror twoP)
  where
    name n = renamed [Replace n] . smartBorders
    tiled = Tall 2 (10/100) (1/2)
    -- tiled = Dishes 2 (10/100)
    tiledG = Tall 1 (5/100) (9/10) -- (g/(1+g))
    g= 1.61 -- Golden ratio
    twoP = TwoPane (3/100) (1/2)
       
extraWs = "abcdghijkmostuvxyz"

myXmobarHook xmproc =  do
  -- workspace containing the focused window
  copies <- wsContainingCopies
  -- print in red workspace containing a copy of the focuse window
  let checkTag ws | ws `elem` copies = xmobarColor "orange" "black" $ pad ws 
                  | otherwise = pad ws
  dynamicLogWithPP xmobarPP
        { ppOutput = hPutStrLn xmproc
        , ppTitle = xmobarColor "green" "" . shorten 50
        , ppHidden = checkTag
        }

myDBusHook dbus =  do
  -- workspace containing the focused window
  copies <- wsContainingCopies
  -- print in red workspace containing a copy of the focuse window
  let checkTag ws | ws `elem` copies = pangoColor "orange" $ pad ws
                  | otherwise = pad ws
  dynamicLogWithPP $ (defaultPP)
    { ppOutput   = dbusOutput dbus
    , ppTitle    = pangoSanitize
    , ppCurrent  = (if null (take 1 copies) then pangoColor "green" else pangoColor "orange") . wrap "[" "]" . pangoSanitize 
    , ppVisible  = pangoColor "green" . {- wrap "(" ")" . -} pangoSanitize
   , ppHidden = checkTag
   , ppLayout = pangoColor "darkred"
  }
main = do
  -- xmproc <- spawnPipe "xmobar"
    dbus <- D.connectSession
    getWellKnownName dbus

    let config =  docks $ defaultConfig
                       { manageHook = manageHook defaultConfig
                       , startupHook = adjustEventInput
                       , handleEventHook = focusOnMouseMove
                       , layoutHook = avoidStruts layout
                       , logHook = myDBusHook dbus <+> fadeInactiveLogHook 0.85
                       , modMask = modm     -- Rebind Mod to the Windows key
                       , borderWidth = 2
                       , focusedBorderColor = "#ff0000" -- "#ffffff"
                       , normalBorderColor = "#0000"
                       , workspaces = [ p <> ws
                                      | ws <- map show [1..9] ++ map return extraWs
                                      , p <- "" : map show [1..9]
                                      ]
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
                         , ("c", sendMessage $ JumpToLayout "Dwindle")
                         ] 
                       -- ++ [("c " ++ show i, windows $ copy (show i)) | i <- [1..9]]
                       )
        -- list of command and keymap. the command name will be used as a name (for documentation)
        -- as well as identifier to be ran via dmenu
        -- @ will be replace by the "leader"
        commands = [
                   -- layout
                     ("@f", "Full" , (sendMessage $ ToggleLayout)) -- >> sendMessage ToggleStruts)
                   , ("@S-f", "Grid" , sendMessage $ JumpToLayout "Grid")
                   , ("@h", "Horizontal", sendMessage $ JumpToLayout "Hor")
                   , ("@S-h", "Horizontal Golden", sendMessage $ JumpToLayout "HorG")
                   , ("@v", "Vertical", sendMessage $ JumpToLayout "Ver")
                   , ("@S-v", "Vertical Golden", sendMessage $ JumpToLayout "VerG")
                   , ("@c", "Dwindle", sendMessage $ JumpToLayout "Dwindle")
		   -- , ("@2", "Two Pane Layout", sendMessage $ JumpToLayout "Hor2")
		   , ("@1", "Full Screen", setLimit 1)
		   , ("@2", "Two Panes Limit", setLimit 2)
		   , ("@3", "Three Panes Layout", setLimit 3)
		   , ("@4", "Four Panes Layout", setLimit 4)
		   , ("@5", "Decrease limit", decreaseLimit)
		   , ("@6", "Two Pane Layout", setLimit 6)
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
                     , ("@S-s s", "Sink window", withFocused $ windows . W.sink)
                     , ("@S-s c", "Float window center", withFocused $ windows . flip W.float centerR )
                     , ("@S-s S-c", "Float window center", withFocused $ windows . flip W.float bigCenterR )
                     , ("@S-s h", "Float window center", withFocused $ windows . flip W.float leftR )
                     , ("@S-s i", "Float window center", withFocused $ windows . flip W.float rightR )
                     , ("@ S-g", "Goto window", gotoMenu )
                     , ("@ S-b", "Bring window", bringMenu )
                     , ("@ b", "Bring window next", actionMenu def (\w s -> W.swapMaster $ W.focusDown $ W.shiftMaster $ W.focusWindow w $ bringWindow w s))
                     , ("@ g", "Master window", actionMenu def (\w s -> W.shiftMaster $ W.focusWindow w s))
                     , ("@ d", "Delete window", kill1 )
                     , ("@S-d d", "Delete all copy window", killAllOtherCopies )
                     , ("@S-d S-d", "Delete all workspace windows", killAll)
                     , ("@S-d w", "Delete all workspace windows", killAll >> moveTo Prev NonEmptyWS)
                   --   focus
                     , ("@m", "Focus Master", windows W.focusMaster)
                     , ("@n", "Focus Next", windows W.focusDown)
                     , ("@e", "Focus Previous", rotSlavesDown)
                     , ("@S-e", "Swap Previous", rotSlavesUp)
                     , ("@S-n", "Swap next", windows W.swapUp)
                     , ("@S-m", "Swap master", windows swapMasterOrShift)
                     , ("@w w", "Focus Next", promote) -- windows W.focusDown)
                   -- toggle transparency
                       , ("@ u", "transparency", spawn "xcompmgr") -- =<< asks (terminal . XMonad.config))
                       , ("@ S-u", "no transparency", spawn "killall xcompmgr") -- =<< asks (terminal . XMonad.config))
                    
                   -- applications
                       , ("@ a t", "terminal", spawn "gnome-terminal") -- =<< asks (terminal . XMonad.config))
                       , ("@ a r", "terminal", spawn "roxterm") -- =<< asks (terminal . XMonad.config))
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
                                                    , ("@ t S-", "Put all and go", [copyAll, W.greedyView])
                                                    , ("@ S-t S-", "Put all", [copyAll])
                                                    , ("@ p S-", "Push all and go", [shiftAll, W.greedyView])
                                                    , ("@ S-p S-", "Push all", [shiftAll])
                                                    , ("@ o ", "Swap all", [swapAll])
                                                    , ("@ S-o ", "Swap all and go", [swapAll, W.greedyView])
                                                    , ("@ i ", "Swap all, local", [swapAll'])
                                                    , ("@ S-i ", "Swap all and go, local", [swapAll' , W.greedyView])
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
                                                    , ("@ t S-", "Put all and go", [copyAll, W.greedyView])
                                                    , ("@ S-t S-", "Put all", [copyAll])
                                                    , ("@ p S-", "Push all and go", [shiftAll, W.greedyView])
                                                    , ("@ S-p S-", "Push all", [shiftAll])
                                                    , ("@ o ", "Swap all", [swapAll])
                                                    , ("@ S-o ", "Swap all and go", [swapAll, W.greedyView])
                                                    , ("@ i ", "Swap all, local", [swapAll'])
                                                    , ("@ S-i ", "Swap all and go, local", [swapAll' , W.greedyView])
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
                  | (sk, sc)   <- zip "qwf" [0..]
                  , (key, description, command) <- [("@ l ", "Swith to screen ", W.view)
                                                   ,("@ p ", "Push to screen ", W.shift)
                                                   ]
                  ]
                  ++ -- switch screan
                  [ ("@ S-w", "Next Screen", do
                             wset <- gets windowset
                             case W.visible wset of
                                [] -> return ()
                                (nextScreen:_) -> do
                                   screenWorkspace (W.screen nextScreen) >>= flip whenJust (windows . W.view)
                    )
                  ]
        commands' = [(s ++ " [" ++ k++ "] " , c) | (k,s,c) <- commands, s /= ""]
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


-- copy all windows of the current workspace to the target WS
copyAll:: (Eq s, Eq i, Eq a) => i -> W.StackSet i l a s sd -> W.StackSet i l a s sd
copyAll i stackset = let
  ws = W.index stackset
  actions = [copyWindow w i | w <- ws]
  in foldr ($) stackset actions
  
-- move all current windows to the  target WS
shiftAll i stackset = let
  ws = W.index stackset
  actions = [W.shiftWin i w | w <- ws]
  in foldr ($) stackset actions


-- swap windows between current and target WS
swapAll i stackset = let
  currentWindows = W.index stackset
  currentTag = W.currentTag stackset
  targetWindows = W.index (W.greedyView i stackset)
  actions = [W.shiftWin i w | w <- currentWindows]
            <> [W.shiftWin currentTag w | w <- targetWindows]
  in foldr ($) stackset actions
  
-- like swapAll but use local WS (ie double letter
-- starting with first ones
swapAll' i0 stackset = let
  (c:_) = W.currentTag stackset
  i = case i0 of
      [x] | x `elem` ['1'..'9'] -> [x]
      (_:x:xs) -> c:x:xs -- drop first one if neede
      xs -> c:xs
  -- in swapAll (c : i) stackset
  in swapAll i stackset
  -- in swapAll i0 stackset
killAll = withAll (\w -> do (focus w) >> kill1)

centerR = W.RationalRect (1/4) (1/4) (1/2) (1/2)
bigCenterR = W.RationalRect (1/8) (1/8) (3/4) (3/4)
leftR = W.RationalRect (0) (1/8) (1/2) (3/4)
rightR = W.RationalRect (4/8) (1/8) (1/2) (3/4)



-- from xmonad-log-applet
-- prettyPrinter :: D.Client -> PP
prettyPrinter dbus = defaultPP
    { ppOutput   = dbusOutput dbus
    , ppTitle    = pangoSanitize
    , ppCurrent  = pangoColor "green" . wrap "[" "]" . pangoSanitize
    , ppVisible  = pangoColor "yellow" . wrap "(" ")" . pangoSanitize
    , ppHidden   = const ""
    , ppUrgent   = pangoColor "red"
    -- , ppLayout   = const ""
    , ppSep      = " "
    }

getWellKnownName :: D.Client -> IO ()
getWellKnownName dbus = do
  D.requestName dbus (D.busName_ "org.xmonad.Log")
                [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  return ()

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal "/org/xmonad/Log" "org.xmonad.Log" "Update") {
            D.signalBody = [D.toVariant ("<b>" ++ (UTF8.decodeString str) ++ "</b>")]
        }
    D.emit dbus signal

pangoColor :: String -> String -> String
pangoColor fg = wrap left right
  where
    left  = "<span foreground=\"" ++ fg ++ "\">"
    right = "</span>"

pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
  where
    sanitize '>'  xs = "&gt;" ++ xs
    sanitize '<'  xs = "&lt;" ++ xs
    sanitize '\"' xs = "&quot;" ++ xs
    sanitize '&'  xs = "&amp;" ++ xs
    sanitize x    xs = x:xs


-- Like W.swapMaster but swap with next if master as focus
swapMasterOrShift s = case W.stack $ W.workspace $ W.current s of
  Just (W.Stack _ [] _) -> W.swapMaster . W.focusDown  $ s
  _ -> W.swapMaster s
