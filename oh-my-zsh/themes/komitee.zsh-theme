# setup basic prompt
local user="$(whoami)"

if [ "$user" = "root" ]; then 
    local user_host="%{$fg[yellow]%}_ROOT_%{$reset_color%}@%{$fg[red]%}$(shorthost)%{$reset_color%}"
    local prompt_color=red
elif [ -n "${${usernames[(r)$user]}:+known_user}" ]; then
    local user_host="%{$fg[red]%}%{$fg[yellow]%}@%{$fg[magenta]%}$(shorthost)%{$reset_color%}"
    local prompt_color=white
else 
    local user_host="%{$fg[yellow]%}%n%{$fg[blue]%}@%{$fg[magenta]%}$(shorthost)%{$reset_color%}"
    local prompt_color=yellow
fi

local prompt_char="%{$fg[$prompt_color]%}%#%{$reset_color%}"

jobs_prompt_info() {
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

    echo "${JOBS_S}${JOBS_R}"
}

if [ "$SHLVL" = "0" ] || [ "$SHLVL" = "1" ] || [ "$SHLVL" = "" ]; then
    local shell_level=""
else
    local shell_level="%{$fg[cyan]%}(%{$fg[red]%}$[${SHLVL} - 1]%{$fg[cyan]%})%{$reset_color%}"
fi

# setup command result indicator
local exit_code="%(?,,%{$fg[red]%}[%?]%{$reset_color%})"

# Put it all together
#PROMPT='${exit_code}%{$fg[cyan]%}[${user_host} %{$fg[yellow]%}%c%{$fg[cyan]%}]$(jobs_prompt_info)${shell_level}${prompt_char} '
PROMPT='${exit_code}%{$fg[cyan]%}[${user_host} %{$fg[yellow]%}%2~%{$fg[cyan]%}]$(jobs_prompt_info)${shell_level}${prompt_char} '

# Setup git prompt info
ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[yellow]%}("
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$fg[yellow]%})%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[yellow]%}%{$fg[red]%}!%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_AHEAD="%{$fg[yellow]%}%{$fg[red]%}+%{$reset_color%}"

ZSH_THEME_GIT_PROMPT_UNTRACKED="%{$fg[yellow]%}%{$fg[red]%}u%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_ADDED="%{$fg[yellow]%}%{$fg[red]%}a%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_MODIFIED="%{$fg[yellow]%}%{$fg[red]%}m%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_RENAMED="%{$fg[yellow]%}%{$fg[red]%}r%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DELETED="%{$fg[yellow]%}%{$fg[red]%}d%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_UNMERGED="%{$fg[yellow]%}%{$fg[red]%}U%{$reset_color%}"

# setup right prompt
#RPROMPT='$(vi_mode_prompt_info)$(git_prompt_info)'
function git_prompt_info() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo "$ZSH_THEME_GIT_PROMPT_PREFIX${ref#refs/heads/}$(parse_git_dirty)$(git_prompt_ahead)$(git_prompt_status)$ZSH_THEME_GIT_PROMPT_SUFFIX"
}
RPROMPT='$(vi_mode_prompt_info)$(git_prompt_info)'

# vim: set ft=zsh ts=4 sw=4 et:
