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

# Setup vi mode indicator
#MODE_INDICATOR="%{$fg_bold[yellow]%}<CMD>%{$reset_color%}"

# Setup git prompt info
ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[yellow]%}(git:"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$fg[yellow]%})%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[yellow]%}%{$fg[red]%}!%{$reset_color%}"

# Setup svn prompt info
ZSH_THEME_SVN_PROMPT_PREFIX="%{$fg[yellow]%}(svn:"
ZSH_THEME_SVN_PROMPT_SUFFIX="%{$fg[yellow]%})%{$reset_color%}"
ZSH_THEME_SVN_PROMPT_DIRTY="%{$fg[red]%}!%{$reset_color%}"

function time_indicator() {
    hour=(${(f)"$(date +"%H")"})
    if [ $hour -lt 9 ]; then
        echo "%{$fg[red]%}%@%{$reset_color%} "
    elif [ $hour -gt 17 ]; then
        echo "%{$fg[red]%}%@%{$reset_color%} "
    fi
}

# setup right prompt
RPROMPT='$(vi_mode_prompt_info)'

# vim: set ft=zsh ts=4 sw=4 et:
