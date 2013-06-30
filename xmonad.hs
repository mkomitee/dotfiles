-- mod-shift-return  Launch terminal
-- mod-p             Launch dmenu
-- mod-shift-p       Launch gmrun
-- mod-shift-c       Close the focused window
-- mod-space         Rotate through the available layout algorithms
-- mod-shift-space   Reset the layouts on the current workspace to default
-- mod-n             Resize viewed windows to the correct size
-- mod-tab           Move focus to the next window
-- mod-shift-tab     Move focus to the previous window
-- mod-j             Move focus to the next window
-- mod-k             Move focus to the previous window
-- mod-m             Move focus to the master window
-- mod-return        Swap the focused window and the master window
-- mod-shift-j       Swap the focused window with the next window
-- mod-shift-k       Swap the focused window with the previous window
-- mod-h             Shrink the master area
-- mod-l             Expand the master area
-- mod-t             Push window back into tiling
-- mod-comma         Increment the number of windows in the master area
-- mod-period        Deincrement the number of windows in the master area
-- mod-b             Toggle the status bar gap
-- mod-shift-q       Quit xmonad
-- mod-q             Restart xmonad
-- mod-[1..9]        Switch to workspace N
-- mod-shift-[1..9]  Move client to workspace N
-- mod-{w,e,r}       Switch to physical/Xinerama screens 1, 2, or 3
-- mod-shift-{w,e,r} Move client to screen 1, 2, or 3
-- mod-button1       Set the window to floating mode and move by dragging
-- mod-button2       Raise the window to the top of the stack
-- mod-button3       Set the window to floating mode and resize by dragging

import XMonad
import XMonad.Layout.Spacing
import XMonad.Layout.Grid
import XMonad.Layout.SimplestFloat
import XMonad.Layout.IM
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Hooks.DynamicLog(xmobar)
import Data.Ratio ((%))
import System.IO





layout = tiled ||| Mirror tiled ||| Grid ||| simplestFloat ||| gimp ||| Full
  where
    tiled     = Tall 1 (3/100) (1/2)
    gimp      = withIM (1%7) (Role "gimp-toolbox") $ gimpright
    gimpright = reflectHoriz $ withIM (1%7) (Role "gimp-dock") $ gimpMain
    gimpMain  = ResizableTall 2 (1/118) (11/20) [1] ||| Full



main = do
     xmonad =<< xmobar defaultConfig
            { terminal           = "urxvt256c"
            , modMask            = mod4Mask
            , borderWidth        = 0
            , normalBorderColor  = "#cccccc"
            , focusedBorderColor = "#cd8b00"
            , focusFollowsMouse  = False
            , manageHook = manageDocks
            , layoutHook = avoidStruts( smartSpacing 10 $ layout )
            }
