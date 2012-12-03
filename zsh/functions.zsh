#!/bin/zsh
function vim-clear-cache() {
    find ~/.vimdata -type f -delete
}

function psg() {
    ps auxww | grep --color=always $* | grep -v grep
}

function which_scm() {
    if [ "$1" = '' ]; then
        dir=$(readlink -f .)
    else
        dir=$(readlink -f $1)
    fi
    if [ -d $dir/.git ]; then
        echo "git"
    elif [ -d $dir/.svn ]; then
        echo "svn"
    elif [ -d $dir/CVS ]; then
        echo "cvs"
    elif [ $dir = '/' ]; then
        return
    else
        which_scm $dir/..
    fi
}
