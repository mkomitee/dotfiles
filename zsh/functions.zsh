#!/bin/zsh
function vim-clear-cache() {
    find ~/.vimdata -type f -delete
}

function resume() {
    if [ "$TMUX" = "" ]; then
        local session="$1"
        if [ "$session" = "" ]; then
            session=main
        fi
        tmux attach-session -t $session
        if [ $? != 0 ]; then
            (
                unset KRB5CCNAME
                tmux new-session -s $session
            )
        fi
    else
        echo "Don't nest TMUX sessions"
    fi
}

# When I'm using xmonad and have a WORKSPACE file to work with, support having
# one gvim server per xmonad workspace. Otherwise, just one server period.
function gvim() {
    local server=GVIM
    if [ "$DESKTOP_SESSION" = "xmonad" ]; then
        if [ -f $HOME/.xmonad/WORKSPACE ]; then
            server="GVIM:$(cat $HOME/.xmonad/WORKSPACE)"
        fi
    elif [ "$DESKTOP_SESSION" = "gnome" ]; then
        server="GVIM:$(xdotool get_desktop)"
    fi
    if [[ $# > 0 ]]; then
        command gvim --servername $server --remote-silent $@ || command gvim $@
    else
        command gvim --servername $server || command gvim
    fi
}

function psgrep() {
    ps auxww | grep --color=always $* | grep -v grep
}

function g {
    if [[ $# > 0 ]]; then
        git "$@"
    else
        git status --short --branch
    fi
}

function ss {
    if [[ $# > 0 ]]; then
        sudo "$@"
    else
        sudo -s
    fi
}

function virtualenv {
    local PYTHONDONTWRITEBYTECODE=0
    command virtualenv $*
}

function pip {
    local PYTHONDONTWRITEBYTECODE=0
    command pip $*
}
