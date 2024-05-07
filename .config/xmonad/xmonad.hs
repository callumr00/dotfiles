import XMonad
import XMonad.Layout
import XMonad.ManageHook
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Gaps
import XMonad.Layout.Grid
import XMonad.Util.Run
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.Exit

blue, black :: String
blue = "#61AFEF"
black = "#5C6370"

manageHook :: ManageHook
manageHook = composeAll [isDialog --> doFloat]

logHook xmobarProc = dynamicLogWithPP $ def {
    ppSep = "    ",
    ppWsSep = " ",
    ppCurrent = \_ -> xmobarColor blue "" "◉",
    ppHidden = \_ -> xmobarColor black "" "◉",
    ppHiddenNoWindows = \_ -> xmobarColor black "" "◌",
    ppTitle = const "",
    ppOutput = hPutStrLn xmobarProc
}

layoutHook = gaps [(U, 32)] $ tiled ||| Mirror tiled ||| Full ||| Grid
    where
        tiled   = Tall nmaster delta ratio
        nmaster = 1
        delta   = 5/100
        ratio   = 1/2

keys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $ [
    ((modMask              , xK_Return), spawn $ XMonad.terminal conf),
    ((modMask              , xK_r     ), spawn "rofi -show drun"),
    ((modMask              , xK_c     ), kill),
    ((modMask              , xK_space ), sendMessage NextLayout),
    ((modMask              , xK_j     ), windows W.focusUp),
    ((modMask              , xK_k     ), windows W.focusDown),
    ((modMask              , xK_h     ), sendMessage Shrink),
    ((modMask              , xK_l     ), sendMessage Expand),
    ((modMask              , xK_t     ), withFocused $ windows . W.sink),
    ((modMask              , xK_comma ), sendMessage (IncMasterN 1)),
    ((modMask              , xK_period), sendMessage (IncMasterN (-1))),
    ((modMask .|. shiftMask, xK_j     ), windows W.swapUp),
    ((modMask .|. shiftMask, xK_k     ), windows W.swapDown),
    ((modMask .|. shiftMask, xK_q     ), io exitSuccess)
    ] ++ [
        ((m .|. modMask, k), windows $ f i) |
        (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9],
        (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    ]

mouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
mouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList [
    ((modMask, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster),
    ((modMask, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
    ]

main :: IO ()
main = do
    xmobarProc <- spawnPipe "xmobar ~/.config/xmobar/xmobar.hs"
    xmonad $ def {
        XMonad.workspaces         = map show [1 .. 9 :: Int],
        XMonad.modMask            = mod4Mask,
        XMonad.terminal           = "alacritty",
        XMonad.borderWidth        = 3,
        XMonad.focusFollowsMouse  = True,
        XMonad.clickJustFocuses   = True,
        XMonad.normalBorderColor  = black,
        XMonad.focusedBorderColor = blue,
        XMonad.manageHook         = Main.manageHook,
        XMonad.logHook            = Main.logHook xmobarProc,
        XMonad.layoutHook         = Main.layoutHook,
        XMonad.keys               = Main.keys,
        XMonad.mouseBindings      = Main.mouseBindings
    }
