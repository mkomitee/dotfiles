#!/bin/sh
DATE=$(date "+%m/%d %I:%m %p")
POM=$(pomodoro status)
if [ $? = 0 ]; then
    color=green
else
    color=red
fi
printf "#[fg=$color,bold]$POM #[fg=colour111,bold]$DATE"
