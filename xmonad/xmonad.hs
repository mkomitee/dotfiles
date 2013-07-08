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
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run
import XMonad.Util.Loggers
import Data.Ratio ((%))
import System.IO


layout = tiled ||| Mirror tiled ||| Grid ||| Full
  where
    tiled     = Tall 1 (3/100) (1/2)


myPP = defaultPP { ppCurrent         = xmobarColor "green" ""
                 , ppHidden          = xmobarColor "yellow" ""
                 , ppUrgent          = xmobarColor "red" ""
                 , ppHiddenNoWindows = id
                 , ppTitle           = xmobarColor "green" "" . shorten 80
                 , ppLayout          = (\ x -> pad $ case x of
                                                 "SmartSpacing 10 Tall"        -> "Tall"
                                                 "SmartSpacing 10 Mirror Tall" -> "Wide"
                                                 "SmartSpacing 10 Grid"        -> "Grid"
                                                 "SmartSpacing 10 Full"        -> "Full"
                                                 _                             -> x
                                       )
                 }

main = do
     status <- spawnPipe "xmobar"
     xmonad $ defaultConfig
            { terminal           = "urxvt256c"
            , modMask            = mod4Mask
            , borderWidth        = 0
            , normalBorderColor  = "#cccccc"
            , focusedBorderColor = "#cd8b00"
            , focusFollowsMouse  = False
            , manageHook = manageDocks
            , layoutHook = avoidStruts $ smartSpacing 10 $ layout 
            , logHook = dynamicLogWithPP $ myPP { ppOutput = hPutStrLn status }
            }
