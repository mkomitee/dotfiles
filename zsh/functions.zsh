#!/usr/bin/env zsh

function tmux_resume() {
    if [ "$TMUX" = "" ]; then
        unset KRB5CCNAME
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

function hgrep() {
    noglob fc -E -lm "$*"* 0
}

alias history='hgrep'

function colours() {
    for i in {0..255}; do
        printf "\x1b[38;5;${i}mcolour${i}\n"
    done
}


function =
{
    echo "$@" | bc -l
}

alias calc="="

function git_remote() {
    local remote parts
    remote=$(command git config remote.origin.url)
    if [ $? != 0 ]; then
        return 0
    fi
    parts=("${(@s:/:)${remote}}")
    echo ${parts[-1]}
}
