#!/bin/sh
POM=$(pomodoro.py status)
if [ $? = 0 ]; then
    color=green
else
    color=red
fi
printf "(#[fg=$color,bold]$POM#[default])"
