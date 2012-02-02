case "$TERM" in
    xterm*|rxvt*)
        preexec () {
            if [ -z "$CONSOLE_TASK" ]; then
                print -Pn "\e]0;%n@%m: $1\a"  # xterm
            else
                print -Pn "\e]0;%n@%m: $1 - $CONSOLE_TASK -\a"  # xterm
            fi
        }
        precmd () {
            if [ -z "$CONSOLE_TASK" ]; then
                print -Pn "\e]0;%n@%m: %~\a"  # xterm
            else
                print -Pn "\e]0;%n@%m: %~ - $CONSOLE_TASK -\a"  # xterm
            fi
        }
        ;;
    screen*)
        preexec () {
            local CMD=${1[(wr)^(*=*|sudo|ssh|-*)]}
            echo -ne "\ek$CMD\e\\"
            if [ -z "$CONSOLE_TASK" ]; then
                print -Pn "\e]0;%n@%m: $1\a"  # xterm
            else
                print -Pn "\e]0;%n@%m: $1 - $CONSOLE_TASK -\a"  # xterm
            fi
        }
        precmd () {
            echo -ne "\ekzsh\e\\"
            if [ -z "$CONSOLE_TASK" ]; then
                print -Pn "\e]0;%n@%m: %~\a"  # xterm
            else
                print -Pn "\e]0;%n@%m: %~ - $CONSOLE_TASK -\a"  # xterm
            fi
        }
        ;;
esac
function task() {
    CONSOLE_TASK="$*"
} 
