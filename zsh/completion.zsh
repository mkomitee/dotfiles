unsetopt complete_in_word
unsetopt correct
unsetopt correct_all
unsetopt glob_complete
unsetopt menu_complete
setopt always_to_end
setopt auto_list
setopt auto_menu
setopt auto_param_slash
setopt auto_remove_slash
setopt list_packed
setopt list_types
setopt rec_exact

autoload -U compinit
zmodload -i zsh/complist
compinit -i -u -d "${HOME}/.zcompdumps/${HOST%%.*}-${EUID}-$ZSH_VERSION"

zstyle ':completion:*' list-colors ''
zstyle ':completion:*' verbose on
zstyle ':completion:*' auto-description on

# Start menu completion if there are 2 ambiguous choices
zstyle ':completion:*' menu select=2 

# Each completion type gets a label, ...
# zstyle ':completion:*:descriptions' format 'Completing %d'
# zstyle ':completion:*' group-name ''

# Add some color to process/job lists in kill-completion
zstyle ':completion:*:*:kill:*:processes' list-colors  '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:*:kill:*:jobs' list-colors  '=(#b) #(%[0-9]#)*=0=01;31'

ps -ww >/dev/null 2>&1
if [ $? ]; then
    zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,comm"
else
    zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,comm -w -w"
fi

# Ignore certain files in completion
zstyle ':completion:*:(all-|)files' ignored-patterns '*?.o' '*?~' '*?.pyc' '*?.pyo'


bindkey '^i' complete-word


