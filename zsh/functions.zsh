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


function disambiguate_pwd() {
    # short holds the result we want to print
    # full holds the full path up to the current segment
    # part holds the current segment, will get as few characters as
    # possible from cur, which is the full current segment

    local short full part cur
    local first
    local -a split    # the array we loop over

    # We do the (D) expansion right here and
    # handle it later if it had any effect
    split=(${(s:/:)${(Q)${(D)1:-$PWD}}})

    if [[ $split == "" ]]; then
    REPLY=/
    return 0
    fi

    # Handling. Perhaps NOT use (D) above and check after shortening?
    if [[ $split[1] = \~* ]]; then
    # named directory we skip shortening the first element
    # and manually prepend the first element to the return value
    first=$split[1]
    # full should already contain the first
    # component since we don't start there
    full=$~split[1]
    shift split
    fi

    # we don't want to end up with something like ~/
    if (( $#split > 0 )); then
        part=/
    fi

    for cur ($split[1,-2]) {
    while {
            part+=$cur[1]
            cur=$cur[2,-1]
            local -a glob
            glob=( $full/$part*(-/N) )
            # continue adding if more than one directory matches or
            # the current string is . or ..
            # but stop if there are no more characters to add
            (( $#glob > 1 )) || [[ $part == (.|..) ]] && (( $#cur > 0 ))
            } { # this is a do-while loop
    }
    full+=$part$cur
    short+=$part
    part=/
    }
    REPLY=$first$short$part$split[-1]
    echo $REPLY
}
