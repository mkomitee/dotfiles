#!/usr/bin/env zsh

which fasd > /dev/null 2>&1
if [ $? -eq 0 ]; then
    eval "$(fasd --init auto)"
    unalias a
    unalias d
    unalias f
    unalias s
    unalias sd
    unalias sf
    unalias zz
else
    alias v='echo fasd unavailable'
fi
