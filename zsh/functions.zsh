#!/bin/zsh
function vim-clear-cache() {
    for type in vimviews vimswap vimbackup ctrlp neocomplcache ; do
        find ~/.vimdata/$type -type f -delete
    done
    find ~/.vimdata/ -name 'powerline.cache*' -type f -delete
    find ~/.vimdata/ -name 'yankring*' -type f -delete
}

function psg() {
    ps auxww | grep --color=always $* | grep -v grep
}
