#!/bin/zsh
autoload colors; colors;
setopt prompt_subst
setopt transient_rprompt


if [ $UID -eq 0 ]; then NCOLOR="red"; SYMBOL='#'; else NCOLOR="yellow"; SYMBOL='$' fi

MODE_INDICATOR="%{$fg_bold[red]%}<%{$fg[red]%}<<%{$reset_color%}"

ZSH_THEME_GIT_PROMPT_PREFIX="("
ZSH_THEME_GIT_PROMPT_SUFFIX=") "
ZSH_THEME_GIT_PROMPT_CLEAN=""
ZSH_THEME_GIT_PROMPT_DIRTY="✗"
ZSH_THEME_GIT_PROMPT_AHEAD="+"

function vi_mode_prompt_info() {
  echo "${${KEYMAP/vicmd/$MODE_INDICATOR}/(main|viins)/}"
}

# Functions for git info in my prompt
# get the name of the branch we are on
function git_prompt_info() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  #echo "$ZSH_THEME_GIT_PROMPT_PREFIX${ref#refs/heads/}$(parse_git_dirty)$ZSH_THEME_GIT_PROMPT_SUFFIX"
  echo "$ZSH_THEME_GIT_PROMPT_PREFIX${ref#refs/heads/}$ZSH_THEME_GIT_PROMPT_SUFFIX"
}

# Checks if working tree is dirty
parse_git_dirty() {
  if [[ -n $(git status -s --ignore-submodules=dirty 2> /dev/null) ]]; then
    echo "$ZSH_THEME_GIT_PROMPT_DIRTY"
  else
    echo "$ZSH_THEME_GIT_PROMPT_CLEAN"
  fi
}

# Checks if there are commits ahead from remote
function git_prompt_ahead() {
  if $(echo "$(git log origin/$(current_branch)..HEAD 2> /dev/null)" | grep '^commit' &> /dev/null); then
    echo "$ZSH_THEME_GIT_PROMPT_AHEAD"
  fi
}

# Formats prompt string for current git commit short SHA
function git_prompt_short_sha() {
  SHA=$(git rev-parse --short HEAD 2> /dev/null) && echo "$ZSH_THEME_GIT_PROMPT_SHA_BEFORE$SHA$ZSH_THEME_GIT_PROMPT_SHA_AFTER"
}

# Formats prompt string for current git commit long SHA
function git_prompt_long_sha() {
  SHA=$(git rev-parse HEAD 2> /dev/null) && echo "$ZSH_THEME_GIT_PROMPT_SHA_BEFORE$SHA$ZSH_THEME_GIT_PROMPT_SHA_AFTER"
}


# Get the status of the working tree
git_prompt_status() {
  INDEX=$(git status --porcelain 2> /dev/null)
  STATUS=""
  if $(echo "$INDEX" | grep '^?? ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_UNTRACKED$STATUS"
  fi
  if $(echo "$INDEX" | grep '^A  ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_ADDED$STATUS"
  elif $(echo "$INDEX" | grep '^M  ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_ADDED$STATUS"
  fi
  if $(echo "$INDEX" | grep '^ M ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_MODIFIED$STATUS"
  elif $(echo "$INDEX" | grep '^AM ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_MODIFIED$STATUS"
  elif $(echo "$INDEX" | grep '^ T ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_MODIFIED$STATUS"
  fi
  if $(echo "$INDEX" | grep '^R  ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_RENAMED$STATUS"
  fi
  if $(echo "$INDEX" | grep '^ D ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_DELETED$STATUS"
  elif $(echo "$INDEX" | grep '^AD ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_DELETED$STATUS"
  fi
  if $(echo "$INDEX" | grep '^UU ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_UNMERGED$STATUS"
  fi
  echo $STATUS
}

function shorthost() {
    # echo the first two parts of the host's fqdn
    uname -n | sed 's/\([^\.]*\.[^\.]*\)\..*/\1/'
}

jobs_prompt_info() {
    # Returns a colored string [#][#]
    # The first # is the nuber of suspended jobs, the brackets are red
    # The second # is the number of backgrounded jobs, the brackets are green
    # If a # is 0, it's not displayed
    local JOBS_S_C=$(jobs -s | wc -l | sed -e 's/ //g')
    local JOBS_R_C=$(jobs -r | wc -l | sed -e 's/ //g')
    if [ "$JOBS_S_C" = "0" ] || [ "$JOBS_S_C" = "" ]; then
        local JOBS_S=""
    else
        local JOBS_S="%{$fg[red]%}[%{$fg[yellow]%}${JOBS_S_C}%{$fg[red]%}]%{$reset_color%}"
    fi

    if [ "$JOBS_R_C" = "0" ] || [ "$JOBS_R_C" = "" ]; then
        local JOBS_R=""
    else
        local JOBS_R="%{$fg[green]%}[%{$fg[yellow]%}${JOBS_R_C}%{$fg[green]%}]%{$reset_color%}"
    fi

    local tmp="${JOBS_S}${JOBS_R}"
    if [ "${tmp}" = '' ]; then
        echo ''
    else
        echo "${tmp} "
    fi
}

function prompt_pomodoro() {
  p_status=$(pomodoro status --short)
  if [ $? != 0 ]; then
      echo "%{$fg[red]%}($p_status)%{$reset_color%}"
  else
      echo "%{$fg[green]%}($p_status)%{$reset_color%}"
  fi
}

# Disable default virtualenv prompt info so we can
# do our own
export VIRTUAL_ENV_DISABLE_PROMPT=1
function virtualenv_prompt_info() {
    if [ "$VIRTUAL_ENV" != "" ]; then
        echo '['`basename $VIRTUAL_ENV`'] '
    fi
}

# Helps display previous commands exit code
local exit_code="%(?,,%{$fg[red]%}[%?] %{$reset_color%})"

PROMPT='${exit_code}%{$fg[$NCOLOR]%}$(shorthost) %c $(jobs_prompt_info)$SYMBOL %{$reset_color%}'

RPS1='$(prompt_pomodoro)%{$fg[$NCOLOR]%}$(git_prompt_info)$(virtualenv_prompt_info)%{$reset_color%}$(vi_mode_prompt_info)'

