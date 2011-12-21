if [ $UID -eq 0 ]; then NCOLOR="red"; SYMBOL='#'; else NCOLOR="yellow"; SYMBOL='$' fi

PROMPT='%{$fg[$NCOLOR]%}$(shorthost) %c $(git_prompt_info)$SYMBOL %{$reset_color%}'

ZSH_THEME_GIT_PROMPT_PREFIX="("
ZSH_THEME_GIT_PROMPT_SUFFIX=") "
ZSH_THEME_GIT_PROMPT_CLEAN=""
ZSH_THEME_GIT_PROMPT_DIRTY="✗"

# See http://geoff.greer.fm/lscolors/
export LSCOLORS="exfxcxdxbxbxbxbxbxbxbx"
export LS_COLORS="di=34;40:ln=35;40:so=32;40:pi=33;40:ex=31;40:bd=31;40:cd=31;40:su=31;40:sg=31;40:tw=31;40:ow=31;40:"

