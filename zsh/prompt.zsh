#!/bin/zsh

autoload colors; colors;
setopt prompt_subst
setopt transient_rprompt

function vi_prompt() {
    if [ "$KEYMAP" = 'vicmd' ]; then
        echo "%{$fg[red]%}n %{$reset_color%}"
    else
        echo "%{$fg[green]%}i %{$reset_color%}"
    fi
}

function vi_rprompt() {
    if [ "$KEYMAP" = 'vicmd' ]; then
        echo "%{$fg[red]%}<<<%{$reset_color%} "
    fi
}

function git_prompt() {
    local ref=$(git symbolic-ref HEAD 2> /dev/null)
    if [ "$ref" != "" ]; then
        echo "(${ref#refs/heads/})"
    fi
}

function shorthost() {
    echo "%(!.%{$fg[red]%}.%{$fg[yellow]%})%2m%{$reset_color%}"
}

jobs_prompt() {
    local JOBS=$(jobs | wc -l | sed 's/ //g')
    if [ "$JOBS" != "0" ]; then
        echo "%{$fg[yellow]%}{$JOBS}%{$reset_color%} "
    fi
}

# Disable default virtualenv prompt info so we can do our own
export VIRTUAL_ENV_DISABLE_PROMPT=1
function virtualenv_prompt() {
    if [ "$VIRTUAL_ENV" != "" ]; then
        VENV=$(basename $VIRTUAL_ENV)
        if [ "$VENV" != "$(uname).$(arch)" ]; then
            echo '['`basename $VIRTUAL_ENV`'] '
        fi
    fi
}

function exit_code() {
    echo "%(?,,%{$fg[red]%}[%?] %{$reset_color%})"
}

function history_prompt() {
    echo "!%!"
}

function enable_prompt() {
    unset PROMPT_DISABLED
}

function setup_prompt() {
    if [ "$PROMPT_DISABLED" != "1" ]; then
        PROMPT='$(vi_prompt)$(shorthost) %{$fg[yellow]%}%# %{$reset_color%}'
        RPROMPT="$(vi_rprompt)$(jobs_prompt)$(exit_code)%{$fg_bold[black]%}%3~$(git_prompt) $(virtualenv_prompt)$(history_prompt)%{$reset_color%}"
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

add-zsh-hook precmd setup_prompt
