#!/bin/zsh
function vim-clear-cache() {
    find ~/.vimdata -type f -delete
}

# When I'm using xmonad and have a WORKSPACE file to work with, support having
# one gvim server per xmonad workspace. Otherwise, just one server period.
function gvim() {
    local server=GVIM
    if [ "$DESKTOP_SESSION" = "xmonad" ]; then
        if [ -f $HOME/.xmonad/WORKSPACE ]; then
            server="GVIM:$(cat $HOME/.xmonad/WORKSPACE)"
        fi
    fi
    if [ "$@" = '' ]; then
        command gvim --servername $server || command gvim
    else
        command gvim --servername $server --remote-silent $@ || command gvim $@
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

function s {
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
