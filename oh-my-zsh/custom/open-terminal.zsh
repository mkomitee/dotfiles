function open-terminal() {
    if [ -z "$TERMINAL" ]; then
        xterm &
    elif [ -x $TERMINAL ]; then
        $TERMINAL &
    else
        xterm &
    fi
}
zle -N open-terminal
#bindkey -M vicmd ^N open-terminal
#bindkey -M viins ^N open-terminal
