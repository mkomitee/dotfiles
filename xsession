#!/bin/sh
export LANG=en_US.UTF-8
xmodmap $HOME/.xmodmaprc
xrdb $HOME/.Xresources
exec i3
