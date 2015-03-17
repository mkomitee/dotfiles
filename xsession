#!/bin/sh
export LANG=en_US.UTF-8
xmodmap $HOME/.xmodmaprc
if [ -x $HOME/scripts/xresources ]; then
    $HOME/scripts/xresources
fi
exec i3
