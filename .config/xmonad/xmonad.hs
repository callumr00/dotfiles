import XMonad
import XMonad.Layout
import XMonad.ManageHook
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Grid
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.Exit

-- The default number of workspaces and their names.
workspaces :: [WorkspaceId]
workspaces = map show [1 .. 9 :: Int]

-- Set the key binding for Mod.
defaultModMask :: KeyMask
defaultModMask = mod4Mask -- Rebind Mod to the Super key.

-- Width of the border window in pixels.
borderWidth :: Dimension
borderWidth = 3

-- Border colours for unfocused and focused windows.
normalBorderColor, focusedBorderColor :: String
normalBorderColor = "#1E2127"  -- Unfocused windows.
focusedBorderColor = "#56B6C2" -- Focused windows.

-- Execute arbitrary actions when managing a new window.
manageHook :: ManageHook
manageHook = composeAll [
    isDialog --> doFloat -- Dialog windows are floating by default.
    ]

-- Specify and transform layouts.
layoutHook = tiled ||| Mirror tiled ||| Full ||| Grid -- Tiling options.
    where
        tiled   = Tall nmaster delta ratio
        nmaster = 1     -- Default number of windows in the master pane.
        ratio   = 1/2   -- Default proportion occupied by master pane.
        delta   = 5/100 -- Percent to increment by when resizing panes.

-- The preferred terminal program.
terminal :: String
terminal = "alacritty"

-- Whether focus follows the mouse pointer.
focusFollowsMouse :: Bool
focusFollowsMouse = True

-- Whether a mouse click just focuses or is also passed to the window.
clickJustFocuses :: Bool
clickJustFocuses = True 

-- Key bindings.
keys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $ [
    -- mod-Shift-Enter    Launch alacritty
    ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf),
    -- mod-p              Launch dmenu
    ((modMask              , xK_p     ), spawn "dmenu_run"),
    -- mod-Shift-c        Close/kill the focused window
    ((modMask .|. shiftMask, xK_c     ), kill),
    -- mod-Space          Rotate through the available layout algorithms
    ((modMask              , xK_space ), sendMessage NextLayout),
    -- mod-Shift-Space    Reset the layouts on the current workspace to default
    ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf),
    -- mod-n              Resize/refresh viewed windows to the correct size
    ((modMask              , xK_n     ), refresh),
    -- mod-Shift-/ Show help message with keyboard and mouse bindings.
    ((modMask .|. shiftMask, xK_slash ), helpCommand),
    -- mod-Tab            Move focus to the next window
    ((modMask              , xK_Tab   ), windows W.focusUp),
    -- mod-Shift-Tab      Move focus to the previous window
    ((modMask .|. shiftMask, xK_Tab   ), windows W.focusDown),
    -- mod-j              Move focus to the next window
    ((modMask              , xK_j     ), windows W.focusUp),
    -- mod-k              Move focus to the previous window
    ((modMask              , xK_k     ), windows W.focusDown),
    -- mod-m              Move focus to the master window
    ((modMask              , xK_m     ), windows W.focusMaster),
    -- mod-Return         Swap the focused window and the master window
    ((modMask              , xK_Return), windows W.swapMaster),
    -- mod-Shift-j        Swap the focused window with the next window
    ((modMask .|. shiftMask, xK_j     ), windows W.swapUp),
    -- mod-Shift-k        Swap the focused window with the previous window
    ((modMask .|. shiftMask, xK_k     ), windows W.swapDown),
    -- mod-h              Shrink the master area
    ((modMask              , xK_h     ), sendMessage Shrink),
    -- mod-l              Expand the master area
    ((modMask              , xK_l     ), sendMessage Expand),
    -- mod-t              Push window back into tiling; unfloat and re-tile it
    ((modMask              , xK_t     ), withFocused $ windows . W.sink),
    -- mod-,              Increment the number of windows in the master area
    ((modMask              , xK_comma ), sendMessage (IncMasterN 1)),
    -- mod-.              Deincrement the number of windows in the master area
    ((modMask              , xK_period), sendMessage (IncMasterN (-1))),
    -- mod-Shift-q        Quit xmonad
    ((modMask .|. shiftMask, xK_q     ), io exitSuccess),
    -- mod-q              Restart xmonad
    ((modMask              , xK_q     ), 
        spawn "if type xmonad; then xmonad --recompile && xmonad --restart")
    ] ++ [
    ((m .|. modMask, k), windows $ f i)
    -- mod-[1..9]         Switch to workspace N 
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9],
    -- mod-Shift-[1..9]   Move client to workspace N
          (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    ] ++ [
    ((m .|. modMask, k), screenWorkspace sc >>= flip whenJust (windows . f))
    -- mod-{w,e,r}        Switch to physical/Xinerama screen 1, 2, or 3
        | (k, sc) <- zip [xK_w, xK_e, xK_r] [0..],
    -- mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3
          (f, m)  <- [(W.view, 0), (W.shift, shiftMask)]
    ]
    where 
        helpCommand :: X ()
        helpCommand = xmessage help

-- Mouse bindings.
mouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
mouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList [
    -- mod-button1        Set window to floating mode and move by dragging
    ((modMask, button1), \w -> focus w >> mouseMoveWindow w 
                                       >> windows W.shiftMaster),
    -- mod-button2        Raise window to the top of the stack
    ((modMask, button2), windows . (W.shiftMaster .) . W.focusWindow),
    -- mod-button3        Set window to floating mode and resize by dragging
    ((modMask, button3), \w -> focus w >> mouseResizeWindow w 
                                       >> windows W.shiftMaster)
    ]

-- Apply changes.
main :: IO ()
main = xmonad $ def {
    XMonad.workspaces         = Main.workspaces,
    XMonad.modMask            = Main.defaultModMask,
    XMonad.borderWidth        = Main.borderWidth,
    XMonad.normalBorderColor  = Main.normalBorderColor,
    XMonad.focusedBorderColor = Main.focusedBorderColor,
    XMonad.manageHook         = Main.manageHook,
    XMonad.layoutHook         = Main.layoutHook,
    XMonad.terminal           = Main.terminal,
    XMonad.focusFollowsMouse  = Main.focusFollowsMouse,
    XMonad.clickJustFocuses   = Main.clickJustFocuses,
    XMonad.keys               = Main.keys,
    XMonad.mouseBindings      = Main.mouseBindings
}

-- Help message with keyboard and mouse bindings.
help :: String
help = unlines [
    "The default modifier key is Super.",
    "Default keybindings:",
    "",
    "-- Launching and killing programs",
    "mod-Shift-Enter    Launch alacritty",
    "mod-p              Launch dmenu",
    "mod-Shift-c        Close/kill the focused window",
    "mod-Space          Rotate through the available layout algorithms",
    "mod-Shift-Space    Reset the layouts on the current workspace to default",
    "mod-n              Resize/refresh viewed windows to the correct size",
    "mod-Shift-/        Show help message with keyboard and mouse bindings",
    "",
    "-- Move focus up or down the window stack",
    "mod-Tab            Move focus to the next window",
    "mod-Shift-Tab      Move focus to the previous window",
    "mod-j              Move focus to the next window",
    "mod-k              Move focus to the previous window",
    "mod-m              Move focus to the master window",
    "",
    "-- Modifying the window order",
    "mod-Return         Swap the focused window and the master window",
    "mod-Shift-j        Swap the focused window with the next window",
    "mod-Shift-k        Swap the focused window with the previous window",
    "",
    "-- Resizing the master/slave ratio",
    "mod-h              Shrink the master area",
    "mod-l              Expand the master area",
    "",
    "-- Floating layer support",
    "mod-t              Push window back into tiling; unfloat and re-tile it",
    "",
    "-- Increase or decrease number of windows in the master area",
    "mod-,              Increment the number of windows in the master area",
    "mod-.              Deincrement the number of windows in the master area",
    "",
    "-- Quit, or restart",
    "mod-Shift-q        Quit xmonad",
    "mod-q              Restart xmonad",
    "",
    "-- Workspaces & screens",
    "mod-[1..9]         Switch to workspace N",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screen 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "Mouse bindings",
    "mod-button1        Set the window to floating mode and move by dragging",
    "mod-button2        Raise the window to the top of the stack",
    "mod-button3        Set the window to floating mode and resize by dragging"
    ]
