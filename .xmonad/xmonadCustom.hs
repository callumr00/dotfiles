-- IMPORTS

import XMonad
import Data.Monoid
import System.Exit

import XMonad.Util.Run
import XMonad.Util.SpawnOnce

import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.Grid
import XMonad.Layout.PerWorkspace

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog

import XMonad.Actions.SpawnOn

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
------------------------------------------------------------------------
--  MOD KEY

-- Define mod key; mod1Mask = Left Alt, mod3Mask = Right Alt, mod4Mask = Super
myModMask       = mod4Mask
------------------------------------------------------------------------
-- TERMINAL

-- Define default terminal
myTerminal      = "termite"
------------------------------------------------------------------------
-- WORKSPACES

-- Define workspaces
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]
------------------------------------------------------------------------
-- WINDOWS

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
myBorderWidth   = 3

-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor  = "#353535"
myFocusedBorderColor = "#5296F0"
------------------------------------------------------------------------
--  KEYBINDS

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,               xK_p     ), spawn "dmenu_run")

    -- launch gmrun
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")

    -- close focused window
    , ((modm,               xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

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

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    , ((modm .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ]
    ++
    
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
------------------------------------------------------------------------
-- MOUSE BINDS

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]
------------------------------------------------------------------------
-- LAYOUTS

-- Reset with 'mod-shift-space' after 'mod-q' restart to apply

myLayout = gaps [(L,12), (R,12), (U,36), (D,12)] $ spacing 12 $ onWorkspace "2" (Mirror (Tall 0 delta ratio)) (tiled ||| Mirror tiled ||| Full ||| Grid)
  where
     -- Default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Percent of screen to increment by when resizing panes
     delta   = 5/100

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
------------------------------------------------------------------------
-- WINDOW RULES

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = manageSpawn <+> composeAll
    [ className =? "MPlayer"        --> doFloat
    , isDialog                      --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]
------------------------------------------------------------------------
-- EVENT HANDLING

-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty
------------------------------------------------------------------------
-- STATUS BARS AND LOGGING

-- Perform an arbitrary action on each internal state change or X event.

myLogHook h = dynamicLogWithPP $ def
    { ppCurrent = xmobarColor "#1bb817" "" . wrap "[" "]"
    , ppHidden = xmobarColor "#5296f0" ""
    , ppHiddenNoWindows = xmobarColor "#7f8080" ""
    , ppTitle = xmobarColor "#ffffff" "" . shorten 60
    , ppLayout = xmobarColor "#ffffff" ""
    , ppUrgent = xmobarColor "#e3411c" ""
    , ppSep = "<fc=#ffffff> | </fc>"
    , ppOutput = hPutStrLn h 
    } 
------------------------------------------------------------------------
-- STARTUP HOOK

-- Perform when xmonad is started / restarted

myStartupHook = composeAll
      [ spawnOnce "xmobar &"
      , spawnOnce "nitrogen --restore &"
      , spawnOnce "picom &"
      , spawnOn "1" "spotify"
      , spawnOn "2" "termite"
      , spawnOn "2" "termite -e ncdu"
      , spawnOn "2" "termite -e htop" ]
------------------------------------------------------------------------
-- SETTINGS

-- Run xmonad with the settings specified
main = do
      xmobarProc <- spawnPipe "xmobar /home/callum/.config/xmobar/xmobarrc"
      xmonad $ docks $ def {
        -- General; Terminal, Modkey, Workspaces and Windows
          terminal           = myTerminal,
          focusFollowsMouse  = myFocusFollowsMouse,
          clickJustFocuses   = myClickJustFocuses,
          borderWidth        = myBorderWidth,
          modMask            = myModMask,
          workspaces         = myWorkspaces,
          normalBorderColor  = myNormalBorderColor,
          focusedBorderColor = myFocusedBorderColor,

        -- Key and Mouse Bindings
          keys               = myKeys,
          mouseBindings      = myMouseBindings,

        -- Hooks and Layouts
          layoutHook         = myLayout,
          manageHook         = myManageHook,
          handleEventHook    = myEventHook,
          logHook            = myLogHook xmobarProc,
          startupHook        = myStartupHook
      }
------------------------------------------------------------------------
-- HELP MENU

-- Commands and their corresponding actions

help :: String
help = unlines ["Mod key = 'Super'. Keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch termite",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
    "mod-c      Close/kill the focused window",
    "mod-Space        Rotate through layouts",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
