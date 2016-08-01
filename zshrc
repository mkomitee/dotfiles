#!/usr/bin/env zsh
DOT=$HOME/.dotfiles
ZSH=$DOT/zsh
CONTRIB=$DOT/contrib
HISTFILE=$HOME/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

hash -d dot=$DOT

source $ZSH/spectrum.zsh
source $ZSH/aliases.zsh
source $ZSH/key-bindings.zsh
source $ZSH/functions.zsh
source $ZSH/prompt.zsh

# LS CONFIG
# Find the option for using colors in ls, depending on the version: Linux or BSD
if ls --color -d . &>/dev/null; then
    alias ls='ls -F --color=tty'
else
    alias ls='ls -FG'
fi


setopt always_to_end
setopt append_history
setopt auto_continue
setopt auto_list
setopt auto_menu
setopt auto_param_slash
setopt auto_pushd
setopt bg_nice
setopt case_glob
setopt case_glob
setopt case_match
setopt cdable_vars
setopt check_jobs
setopt complete_in_word
setopt extended_history
setopt glob_assign
setopt glob_complete
setopt hist_expire_dups_first
setopt hist_find_no_dups
setopt hist_ignore_all_dups
setopt hist_ignore_space
setopt hist_reduce_blanks
setopt hist_verify
setopt inc_append_history
setopt interactive_comments
setopt list_packed
setopt list_types
setopt long_list_jobs
setopt mark_dirs
setopt no_nomatch
setopt numericglobsort
setopt pushd_ignore_dups
setopt rc_quotes
setopt short_loops
unsetopt auto_remove_slash
unsetopt beep
unsetopt bg_nice
unsetopt correct
unsetopt correct_all
unsetopt flowcontrol
unsetopt hup
unsetopt menu_complete
unsetopt rec_exact

# This makes esc quicker.
KEYTIMEOUT=1

autoload -Uz compinit
zmodload -i zsh/complist
mkdir -p ${HOME}/.zcompdumps
compinit -i -u -d "${HOME}/.zcompdumps/${HOST%%.*}-${EUID}-$ZSH_VERSION"

zstyle ':completion:*:*:*:*:*' menu select
zstyle ':completion:*:matches' group 'yes'
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'
zstyle ':completion:*:corrections' format ' %F{green}-- %d (errors: %e) --%f'
zstyle ':completion:*:descriptions' format ' %F{yellow}-- %d --%f'
zstyle ':completion:*:messages' format ' %F{purple} -- %d --%f'
zstyle ':completion:*:warnings' format ' %F{red}-- no matches found --%f'
zstyle ':completion:*:default' list-prompt '%S%M matches%s'
zstyle ':completion:*' format ' %F{yellow}-- %d --%f'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' verbose yes

# Don't complete unavailable commands.
zstyle ':completion:*:functions' ignored-patterns '(_*|pre(cmd|exec))'

# Array completion element sorting.
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters

# Directories
zstyle ':completion:*:default' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*:*:cd:*' tag-order local-directories directory-stack path-directories
zstyle ':completion:*:*:cd:*:directory-stack' menu yes select
zstyle ':completion:*:-tilde-:*' group-order 'named-directories' 'path-directories' 'users' 'expand'
zstyle ':completion:*' squeeze-slashes true

# Kill
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,command -w -w"
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:*:kill:*' force-list always
zstyle ':completion:*:*:kill:*' insert-ids single

# Ignores all usernames which start with _ (mostly relevant on osx)
zstyle ':completion:*:*:*:users' ignored-patterns '_*'

# Man
zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:*:manuals.(^1*)' insert-sections true

# SSH/SCP/RSYNC
zstyle ':completion:*:(scp|rsync):*' tag-order 'hosts:-host:host hosts:-domain:domain hosts:-ipaddr:ip\ address *'
zstyle ':completion:*:(scp|rsync):*' group-order users files all-files hosts-domain hosts-host hosts-ipaddr
zstyle ':completion:*:ssh:*' tag-order 'hosts:-host:host hosts:-domain:domain hosts:-ipaddr:ip\ address *'
zstyle ':completion:*:ssh:*' group-order users hosts-domain hosts-host users hosts-ipaddr
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-host' ignored-patterns '*(.|:)*' loopback ip6-loopback localhost ip6-localhost broadcasthost
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-domain' ignored-patterns '<->.<->.<->.<->' '^[-[:alnum:]]##(.[-[:alnum:]]##)##' '*@*'
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-ipaddr' ignored-patterns '^(<->.<->.<->.<->|(|::)([[:xdigit:].]##:(#c,2))##(|%*))' '127.0.0.<->' '255.255.255.255' '::1' 'fe80::*'

# cache expensive completion data
zstyle ':completion::complete:*' use-cache on

# Ignore certain files in completion
zstyle ':completion:*:*:*:*files' ignored-patterns '*?.o' '*?~' '*?.pyc' '*?.pyo'

# quote pasted URLs
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

# Source local config
if [ -f $HOME/.zshrc.local ]; then
    source $HOME/.zshrc.local
fi
