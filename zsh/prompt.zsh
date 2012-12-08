#!/bin/zsh

autoload colors; colors;
setopt prompt_subst
setopt transient_rprompt

function git_prompt() {
    local ref
    ref=$(git symbolic-ref HEAD 2> /dev/null)
    ref="${ref#refs/heads/}"
    if [ "$ref" != "" ]; then
        local clean
        local cached
        local echoed
        clean=$(git diff --shortstat 2> /dev/null)
        cached=$(git diff --cached --shortstat 2> /dev/null)
        if [ "$clean" != "" ]; then
            echo -n "%{$fg_bold[red]%}+%{$reset_color%}"
            echoed=1
        elif [ "$cached" != "" ]; then
            echo -n "%{$fg_bold[yellow]%}+%{$reset_color%}"
            echoed=1
        fi
        if [ "$ref" != "master" ]; then
            echo -n "%{$fg_bold[black]%}($ref)%{$reset_color%}"
            echoed=1
        fi
        if [ -n $echoed ]; then
            echo -n ' '
        fi
    fi
}

function shorthost() {
    if [ "$KEYMAP" = 'vicmd' ]; then
        echo "%{$fg[blue]%}%2m%{$reset_color%}"
    elif [ "$USER" = 'root' ]; then
        echo "%{$fg[red]%}%2m%{$reset_color%}"
    else
        echo "%{$fg[yellow]%}%2m%{$reset_color%}"
    fi
}

function prompt() {
    if [ "$USER" = 'root' ]; then
        echo "%{$fg[red]%}%#%{$reset_color%}"
    else
        echo "%{$fg[yellow]%}%#%{$reset_color%}"
    fi
}


function jobs_prompt() {
    local JOBS=$(jobs | wc -l | sed 's/ //g')
    if [ "$JOBS" != "0" ]; then
        echo "%{$fg_bold[black]%}($JOBS)%{$reset_color%} "
    fi
}

# Disable default virtualenv prompt info so we can do our own
export VIRTUAL_ENV_DISABLE_PROMPT=1
function virtualenv_prompt() {
    if [ "$VIRTUAL_ENV" != "" ]; then
        VENV=$(basename $VIRTUAL_ENV)
        if [ "$VENV" != "$(uname).$(arch)" ]; then
            echo "[$VENV] "
        fi
    fi
}

function exit_code() {
    echo "%(?,,%{$fg[red]%}[%?]%{$reset_color%} )"
}

function history_prompt() {
    echo "%{$fg_bold[black]%}!%!%{$reset_color%}"
}

function enable_prompt() {
    unset PROMPT_DISABLED
}

function current_dir() {
    echo "%{$fg_bold[black]%}%3~%{$reset_color%}"
}

function setup_prompt() {
    if [ "$PROMPT_DISABLED" != "1" ]; then
        PROMPT='$(exit_code)$(shorthost) $(git_prompt)$(current_dir) $(jobs_prompt)$(prompt) '
        RPROMPT="$(history_prompt)"
    fi
}

function disable_prompt() {
    PROMPT_DISABLED=1
    unset RPROMPT
    PROMPT='%# '
}

function toggle_prompt() {
    if [ -z "$RPROMPT" ]; then
        enable_prompt
    else
        disable_prompt
    fi
}

autoload -U add-zsh-hook
add-zsh-hook precmd setup_prompt
