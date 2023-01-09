import XMonad
import Data.Monoid
import System.Exit
import XMonad.Actions.SpawnOn
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.Grid
import XMonad.Layout.PerWorkspace
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

myModMask :: KeyMask
-- myModMask       = mod1Mask -- Left Alt
-- myModMask       = mod3Mask -- Right Alt
myModMask       = mod4Mask -- Super Key

myTerminal :: String
myTerminal      = "alacritty"

myWorkspaces    = [" 1 "," 2 "," 3 "," 4 "," 5 "," 6 "," 7 "," 8 "," 9 "]

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
myBorderWidth :: Dimension
myBorderWidth = 3 

-- Window border colour
myNormalBorderColor :: String
myNormalBorderColor  = "#e0f2fc" -- Unfocused

myFocusedBorderColor :: String
myFocusedBorderColor = "#64b9d8" -- Focused

myKeyBindings conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- Launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- Launch dmenu
    , ((modm,               xK_p     ), spawn "dmenu_run")

    -- Close focused window
    , ((modm,               xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_k     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_j     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapUp    )

    -- Shrink the master pane
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master pane
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++
    
    -- Switch to workspace N: mod & [1..9]
    -- Move window to workspace N: mod & shift & [1..9]
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

myMouseBindings conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- Set the window to floating and move by dragging: mod & left click
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    -- Raise the window to the top of the stack: mod & scroll wheel click
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- Set the window to floating and resize by dragging: mod & right click
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]

-- myManageHook :: ManageHook
myManageHook = manageSpawn <+> composeAll
    [ className =? "MPlayer"        --> doFloat
    , isDialog                      --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore 
    ]

-- Restart and apply: mod & q -> mod & shift & space
myLayoutHook =
  gaps [
     (L, 12) -- Left padding
    ,(R, 12) -- Right padding
    ,(U, 36) -- Up (Top) padding
    ,(D, 12) -- Down (Bottom) padding
  ]
  $ spacing 12 -- Window padding
  $ onWorkspace " 2 " (Mirror (Tall 0 delta ratio)) -- Set workspace 2 layout
  $ tiled ||| Mirror tiled ||| Full ||| Grid -- Set layout options
  where
    tiled = Tall nmaster delta ratio -- Set tiled layout settings

    nmaster = 1      -- Default number of windows in the master pane
    delta   = 5/100  -- Percent of screen to increment by when resizing panes
    ratio   = 1/2    -- Default proportion of screen occupied by master pane

myLogHook h = dynamicLogWithPP $ def
    { ppCurrent = xmobarColor "#64b9d8" "" . wrap "[" "]" -- Active workspace
    , ppHidden = xmobarColor "#e0f2fc" "" -- Non-empty & Inactive Workspace
    , ppHiddenNoWindows = xmobarColor "#20364d" "" -- Empty & Inactive Workspace
    , ppTitle = xmobarColor "#e0f2fc" "" . shorten 602
    , ppLayout = xmobarColor "#e0f2fc" ""
    , ppUrgent = xmobarColor "#e3411c" ""
    , ppSep = "<fc=#ffffff> | </fc>"
    , ppOutput = hPutStrLn h 
    } 

-- Perform when xmonad is started / restarted
myStartupHook :: X ()
myStartupHook = do
  spawnOnce "xmobar &"
  spawnOnce "alacritty -e wal -R" 
  spawnOn " 1 " "spotify"
  spawnOn " 2 " "alacritty -e 'sudo pacman -Sy'"
  spawnOn " 2 " "alacritty -e htop"
  spawnOn " 2 " "alacritty -e ncdu"
  spawnOn " 2 " "psensor"

main = do
      xmobarProc <- spawnPipe "xmobar /home/callum/.config/xmobar/xmobarrc"
      xmonad $ docks $ def {
          terminal           = myTerminal,
          focusFollowsMouse  = myFocusFollowsMouse,
          clickJustFocuses   = myClickJustFocuses,
          borderWidth        = myBorderWidth,
          modMask            = myModMask,
          workspaces         = myWorkspaces,
          normalBorderColor  = myNormalBorderColor,
          focusedBorderColor = myFocusedBorderColor,
          keys               = myKeyBindings,
          mouseBindings      = myMouseBindings,
          layoutHook         = myLayoutHook,
          manageHook         = myManageHook,
          logHook            = myLogHook xmobarProc,
          startupHook        = myStartupHook
      }