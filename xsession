#!/bin/sh
export LANG=en_US.UTF-8
xmodmap $HOME/.xmodmaprc
xrdb $HOME/.Xresources
xrdb -override $HOME/X/base16-default.dark.256.xresources
if [ -f $HOME/.Xresources.local ]; then
    xrdb -override $HOME/.Xresources.local
fi
exec i3
