#!/bin/sh

# Sets the background
which feh > /dev/null 2>&1
if [ $? = 0 ]; then
    if [ -f $HOME/Pictures/Sleeper-manip.jpg ]; then
        feh --bg-scale --no-fehbg $HOME/Pictures/Sleeper-manip.jpg
    fi
fi

# Starts a compositing manager
which xcompmgr > /dev/null 2>&1
if [ $? = 0 ]; then
    xcompmgr -Ffn &
fi

# Hides the mouse when idle
which unclutter > /dev/null 2>&1
if [ $? = 0 ]; then
    unclutter -idle 1 &
fi

# Start the urxvt daemon
which urxvt256cd > /dev/null 2>&1
if [ $? = 0 ]; then
    urxvt256cd -q -f -o
fi

# Simple panel to use as a systray
# which xfce4-panel > /dev/null 2>&1
# if [ $? = 0 ]; then
    # xfce4-panel --disable-wm-check &
# fi

exec xmonad
