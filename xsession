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
    TERMINAL=urxvt256cc
    urxvt256cd -q -f -o
else
    TERMINAL=urxvtc
    urxvtd -q -f -o
fi
export TERMINAL

eval $(ssh-agent)

exec xmonad
