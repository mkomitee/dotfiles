#!/bin/zsh
function vim-clear-cache() {
    find ~/.vimdata -type f -delete
}

function psg() {
    ps auxww | grep --color=always $* | grep -v grep
}
