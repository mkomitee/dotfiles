#!/usr/bin/env zsh

function tmux_resume() {
    if [ "$TMUX" = "" ]; then
        unset KRB5CCNAME
        unset DIDZSHENV
        unset DIDZSHRC
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
        sudo -E "$@"
    else
        sudo -E -s
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

function fps() {
    ps "$@" | fzf -i -e --header-lines=1
}

function ffind()
{
    find "$@" | fzf -i -e
}

function fdind()
{
    fd | fzf -i -e
}

function gfind()
{
    git ls-files | fzf -i -e
}

function man() {
    env \
        LESS_TERMCAP_mb=$(printf "\e[1;31m") \
        LESS_TERMCAP_md=$(printf "\e[1;31m") \
        LESS_TERMCAP_me=$(printf "\e[0m") \
        LESS_TERMCAP_se=$(printf "\e[0m") \
        LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
        LESS_TERMCAP_ue=$(printf "\e[0m") \
        LESS_TERMCAP_us=$(printf "\e[1;32m") \
        man "$@"
}

function perldoc() {
    env \
        LESS_TERMCAP_mb=$(printf "\e[1;31m") \
        LESS_TERMCAP_md=$(printf "\e[1;31m") \
        LESS_TERMCAP_me=$(printf "\e[0m") \
        LESS_TERMCAP_se=$(printf "\e[0m") \
        LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
        LESS_TERMCAP_ue=$(printf "\e[0m") \
        LESS_TERMCAP_us=$(printf "\e[1;32m") \
        perldoc "$@"
}
