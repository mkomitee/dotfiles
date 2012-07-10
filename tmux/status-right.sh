#!/bin/sh
DATE=$(date "+%m/%d %I:%M %p")
POM=$(pomodoro.py status)
if [ $? = 0 ]; then
    color=green
else
    color=red
fi
printf "#[fg=$color,bold]$POM #[fg=colour111,bold]$DATE"
