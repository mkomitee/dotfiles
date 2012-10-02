setopt always_to_end
setopt auto_list
setopt auto_menu
setopt auto_param_slash
setopt auto_remove_slash
setopt complete_in_word
setopt list_packed
setopt list_types
setopt rec_exact
unsetopt correct
unsetopt correct_all
unsetopt glob_complete
unsetopt menu_complete

autoload -U compinit
zmodload -i zsh/complist
compinit -i -u -d "${HOME}/.zcompdumps/${HOST%%.*}-${EUID}-$ZSH_VERSION"

zstyle ':completion:*' list-colors ''
zstyle ':completion:*' verbose on
zstyle ':completion:*' auto-description on

# Case sensitive completion
zstyle ':completion:*' matcher-list 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

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

[ -r /etc/ssh/ssh_known_hosts ] && _global_ssh_hosts=(${${${${(f)"$(</etc/ssh/ssh_known_hosts)"}:#[\|]*}%%\ *}%%,*}) || _ssh_hosts=()
[ -r ~/.ssh/known_hosts ] && _ssh_hosts=(${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[\|]*}%%\ *}%%,*}) || _ssh_hosts=()
[ -r /etc/hosts ] && : ${(A)_etc_hosts:=${(s: :)${(ps:\t:)${${(f)~~"$(</etc/hosts)"}%%\#*}##[:blank:]#[^[:blank:]]#}}} || _etc_hosts=()

hosts=(
  "$_global_ssh_hosts[@]"
  "$_ssh_hosts[@]"
  "$_etc_hosts[@]"
  "$HOST"
  localhost
)
zstyle ':completion:*:hosts' hosts $hosts

zstyle ':completion::complete:*' use-cache 1
zstyle ':completion::complete:*' cache-path ~/.zcompcache/

zstyle ':completion:*:*:*:users' ignored-patterns '_*'

expand-or-complete-with-dots() {
    echo -n "\e[31m......\e[0m"
    zle expand-or-complete
    zle redisplay
}
zle -N expand-or-complete-with-dots
bindkey "^I" expand-or-complete-with-dots
