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

function v {
    vim ${@:-'.'}
}
