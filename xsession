#!/bin/sh
export LANG=en_US.UTF-8
xmodmap $HOME/.xmodmaprc
xrdb $HOME/.Xresources
if [ -f $HOME/.Xresources.local ]; then
    xrdb -override $HOME/.Xresources.local
fi
exec i3
