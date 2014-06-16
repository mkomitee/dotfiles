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
        local ahead
        local behind
        local echoed

        clean=$(git diff --shortstat 2> /dev/null)
        cached=$(git diff --cached --shortstat 2> /dev/null)
        remote=${$(git rev-parse --verify ${hook_com[branch]}@{upstream} --symbolic-full-name 2>/dev/null)/refs\/remotes\/}
        if [ -n "$remote" ]; then
            ahead=$(git rev-list ${hook_com[branch]}@{upstream}..HEAD 2>/dev/null | wc -l)
            behind=$(git rev-list HEAD..${hook_com[branch]}@{upstream} 2>/dev/null | wc -l)
        fi
        if [ "$clean$cached$ahead$behind" != "00" ]; then
            echo -n "%{$fg_bold[red]%}+%{$reset_color%}"
            echoed=1
        fi
        if [ "$ref" != "master" ]; then
            echo -n "%{$fg[yellow]%}($ref)%{$reset_color%}"
            echoed=1
        fi
        if [ -n "$echoed" ]; then
            echo -n ' '
        fi
    fi
}

function shorthost() {
    if [ "$KEYMAP" = 'vicmd' ]; then
        echo "%{$fg[blue]%}%2m%{$reset_color%}"
    else
        echo "%(!.%{$fg[red]%}.%{$fg[yellow]%})%2m%{$reset_color%}"
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
        echo "%{$fg[yellow]%}($JOBS)%{$reset_color%} "
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
    echo "%{$fg[yellow]%}!%!%{$reset_color%}"
}

function enable_prompt() {
    unset PROMPT_DISABLED
}

function current_dir() {
    echo "%{$fg[yellow]%}%3~%{$reset_color%}"
}

function setup_prompt() {
    if [ "$PROMPT_DISABLED" != "1" ]; then
        PROMPT='$(exit_code)$(shorthost) $(virtualenv_prompt)$(git_prompt)$(current_dir) $(jobs_prompt)$(prompt) '
    else
        PROMPT='$(exit_code)$(shorthost) $(virtualenv_prompt)$(current_dir) $(jobs_prompt)$(prompt) '
    fi
}

function disable_prompt() {
    PROMPT_DISABLED=1
}

function toggle_prompt() {
    if [ -z "$PROMPT_DISABLED" ]; then
        disable_prompt
    else
        enable_prompt
    fi
}

function title {
  [ "$TERM" != "linux" ] || return
  [ "$DISABLE_AUTO_TITLE" != "true" ] || return
  printf '\033]2;%s\033\\' "$1"
}

# Set the window title (tmux pane) to indicate we're ready for a command, also
# setup the prompt.
function my_precmd {
  setup_prompt
  title "shell"
}

# Set the window title (tmux pane) to the command we're running. This is for vim
# & window pane navigation integration
function my_preexec {
  title "$1"
}

autoload -U add-zsh-hook
add-zsh-hook precmd  my_precmd
add-zsh-hook preexec my_preexec
