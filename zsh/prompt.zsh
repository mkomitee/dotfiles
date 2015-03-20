#!/usr/bin/env zsh

# Disable default virtualenv prompt info
export VIRTUAL_ENV_DISABLE_PROMPT=1

autoload colors; colors;
setopt prompt_subst
setopt transient_rprompt
MKPROMPT_TIMER=0
MKPROMPT_CUTOFF=10

function mkprompt_host() {
    if [ "$KEYMAP" = 'vicmd' ]; then
        echo "%{$fg[blue]%}%2m%{$reset_color%}"
    else
        echo "%(!.%{$fg[red]%}.%{$fg[yellow]%})%2m%{$reset_color%}"
    fi

}

function mkprompt_prompt() {
    if [ "$USER" = 'root' ]; then
        echo "%{$fg[red]%}%#%{$reset_color%}"
    else
        echo "%{$fg[yellow]%}%#%{$reset_color%}"
    fi
}

function mkprompt_jobs() {
    local JOBS=$(jobs | wc -l | sed 's/ //g')
    if [ "$JOBS" != "0" ]; then
        echo "%{$fg[yellow]%}($JOBS)%{$reset_color%} "
    fi
}

function mkprompt_exit() {
    echo "%(?,,%{$fg[red]%}[%?]%{$reset_color%} )"
}

function mkprompt_cwd() {
    echo "%{$fg[yellow]%}%3~%{$reset_color%}"
}

function mkprompt_timer() {
    MKPROMPT_TIMER=$(date +%s)
}

function mkprompt_elapsed() {
    if [[ ${MKPROMPT_TIMER} -ne 0 ]]; then
        local elapsed=$(($(date +%s) - $MKPROMPT_TIMER))
        if [[ $elapsed -ge  ${MKPROMPT_CUTOFF} ]]; then
            local hours=$((${elapsed}/3600))
            local remainder=$((${elapsed}%3600))
            local minutes=$((${remainder}/60))
            local seconds=$((${remainder}%60))
            local clock=$(printf '%d:%02d:%02d' $hours $minutes $seconds)
            echo "%{$fg[red]%}[${clock}]%{$reset_color%} "
        fi
    fi
    MKPROMPT_TIMER=0
}

function mkprompt_setup() {
    PROMPT=""
    PROMPT="${PROMPT}$(mkprompt_exit)"
    PROMPT="${PROMPT}$(mkprompt_elapsed)"
    PROMPT="${PROMPT}$(mkprompt_host) "
    PROMPT="${PROMPT}$(mkprompt_cwd) "
    PROMPT="${PROMPT}$(mkprompt_jobs)"
    PROMPT="${PROMPT}$(mkprompt_prompt) "
}

autoload -U add-zsh-hook
add-zsh-hook precmd mkprompt_setup
add-zsh-hook preexec mkprompt_timer
