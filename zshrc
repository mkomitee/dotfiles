#!/usr/bin/env zsh
DOT=$HOME/.dotfiles
ZSH=$DOT/zsh
CONTRIB=$DOT/contrib
HISTFILE=$HOME/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

hash -d dot=$DOT

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
setopt hist_ignore_all_dups
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
setopt extended_history
setopt glob_assign
setopt glob_complete
setopt hist_expire_dups_first
setopt hist_find_no_dups
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
setopt share_history
setopt short_loops
unsetopt auto_remove_slash
unsetopt beep
unsetopt complete_in_word
unsetopt correct
unsetopt correct_all
unsetopt flowcontrol
unsetopt hup
unsetopt menu_complete
unsetopt rec_exact

KEYTIMEOUT=20

autoload -U compinit
zmodload -i zsh/complist
compinit -i -u -d "${HOME}/.zcompdumps/${HOST%%.*}-${EUID}-$ZSH_VERSION"

# Stolen from oh-my-zsh
zstyle ':completion:*:*:*:*:*' menu select

# colorizes completion menu for kill command
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'

# Includes additional information in completion menu for everything that lists
# processes (e.g. kill)
zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,args -w -w"

# Ignores all usernames which start with _ (mostly relevant on osx)
zstyle ':completion:*:*:*:users' ignored-patterns '_*'

# cache expensive completion data
zstyle ':completion::complete:*' use-cache 1

zstyle ':completion:*' auto-description on

# Start menu completion if there are 2 ambiguous choices
zstyle ':completion:*' menu select=2

# Remove trailing slashes in directory completion
zstyle ':completion:*' squeeze-slashes true

# Ignore certain files in completion
zstyle ':completion:*:*:*:*files' ignored-patterns '*?.o' '*?~' '*?.pyc' '*?.pyo'

# Colorize completion for files
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"

# quote pasted URLs
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

# Source local config
if [ -f $HOME/.zshrc.local ]; then
    source $HOME/.zshrc.local
fi
