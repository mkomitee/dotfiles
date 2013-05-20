#!/bin/zsh
function vim-clear-cache() {
    find ~/.vimdata -type f -delete
}

function psg() {
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
