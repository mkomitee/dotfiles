if [ $UID -eq 0 ]; then NCOLOR="red"; SYMBOL='#'; else NCOLOR="yellow"; SYMBOL='$' fi


ZSH_THEME_GIT_PROMPT_PREFIX="("
ZSH_THEME_GIT_PROMPT_SUFFIX=") "
ZSH_THEME_GIT_PROMPT_CLEAN=""
ZSH_THEME_GIT_PROMPT_DIRTY="✗"
ZSH_THEME_GIT_PROMPT_AHEAD="+"

function git_prompt_info() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo "$ZSH_THEME_GIT_PROMPT_PREFIX${ref#refs/heads/}$(parse_git_dirty)$(git_prompt_ahead)$ZSH_THEME_GIT_PROMPT_SUFFIX"
}

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

    tmp="${JOBS_S}${JOBS_R}"
    if [ "${tmp}" = '' ]; then
        echo ''
    else
        echo "${tmp} "
    fi
}

local exit_code="%(?,,%{$fg[red]%}[%?] %{$reset_color%})"

# See http://geoff.greer.fm/lscolors/
export LSCOLORS="exfxcxdxbxbxbxbxbxbxbx"
export LS_COLORS="di=34;40:ln=35;40:so=32;40:pi=33;40:ex=31;40:bd=31;40:cd=31;40:su=31;40:sg=31;40:tw=31;40:ow=31;40:"

PROMPT='${exit_code}%{$fg[$NCOLOR]%}$(shorthost) %c $(git_prompt_info)$(jobs_prompt_info)$SYMBOL %{$reset_color%}'
