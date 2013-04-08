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

function vim {
    command vim ${@:-'.'}
}

function virtualenv {
    local PYTHONDONTWRITEBYTECODE=0
    command virtualenv $*
}

function pip {
    local PYTHONDONTWRITEBYTECODE=0
    command pip $*
}
