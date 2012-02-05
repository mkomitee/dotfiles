
autoload -U compinit
compinit -i -u -d "${HOME}/.zcompdumps/${HOST%%.*}-${EUID}-$ZSH_VERSION"

unsetopt correct
unsetopt correct_all
unsetopt menu_complete
unsetopt auto_remove_slash
setopt auto_menu
setopt complete_in_word
setopt always_to_end
setopt auto_list
setopt auto_param_slash
setopt glob_complete
setopt list_packed
setopt list_types

WORDCHARS=''

zmodload -i zsh/complist

# Match case sensitively
zstyle ':completion:*' matcher-list 'r:|[._-]=* r:|=*' 

zstyle ':completion:*' list-colors ''

zstyle ':completion:*:*:*:*:*' menu select

# disable named-directories autocompletion
zstyle ':completion:*:cd:*' tag-order local-directories directory-stack path-directories
cdpath=(.)

# Use caching so that commands like apt and dpkg complete are useable
zstyle ':completion::complete:*' use-cache 1

# Tweak completion for kill command
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
# We're interested in all processes, not just mine. I'm an SA
ps -ww >/dev/null 2>&1
if [ $? ]; then
    zstyle ':completion:*:*:*:*:processes' command "ps -e -o pid,user,comm"
else
    zstyle ':completion:*:*:*:*:processes' command "ps -e -o pid,user,comm -w -w"
fi

# On macs, there are lots of users we dont care about
# they all start with _
zstyle ':completion:*:*:*:users' ignored-patterns \
        $(awk -F: '/^_/ { print $1 }' /etc/passwd)

# On macs, there are lots of groups we dont care about
# they all start with _
zstyle ':completion:*:*:*:groups' ignored-patterns \
        $(awk -F: '/^_/ { print $1 }' /etc/group)

# In addition to the default /etc/hosts, ~/.ssh/known_hosts, we parse
# ~/.ssh/config and a magic file ~/.host-completion file for additional hosts
# to complete
[ -f ~/.ssh/config ] && : ${(A)ssh_config_hosts:=${${${${(@M)${(f)"$(<$HOME/.ssh/config)"}:#Host *}#Host }:#*\**}:#*\?*}}
[ -r ~/.ssh/known_hosts ] && _ssh_hosts=(${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[\|]*}%%\ *}%%,*}) || _ssh_hosts=()
[ -r ~/.host-completion ] && : ${(A)_host_completion:=${(s: :)${(ps:\t:)${${(f)~~"$(<$HOME/.host-completion)"}%%\#*}##[:blank:]#[^[:blank:]]#}}} || _host_completion=()
[ -r /etc/hosts ] && : ${(A)_etc_hosts:=${(s: :)${(ps:\t:)${${(f)~~"$(</etc/hosts)"}%%\#*}##[:blank:]#[^[:blank:]]#}}} || _etc_hosts=()
hosts=(
  "$_ssh_config_hosts[@]"
  "$_ssh_hosts[@]"
  "$_etc_hosts[@]"
  "$_host_completion[@]"
  `hostname`
  localhost
)

zstyle ':completion:*:hosts' hosts $hosts
