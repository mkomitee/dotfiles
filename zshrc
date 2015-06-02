#!/usr/bin/env zsh
DOT=$HOME/.dotfiles
ZSH=$DOT/zsh
CONTRIB=$DOT/contrib

hash -d dot=$DOT

source $ZSH/locale.zsh
source $ZSH/completion.zsh
source $ZSH/history.zsh
source $ZSH/aliases.zsh
source $ZSH/key-bindings.zsh
source $ZSH/globbing.zsh
source $ZSH/functions.zsh
source $ZSH/jobs.zsh
source $ZSH/prompt.zsh
source $ZSH/colours.zsh
source $ZSH/extensions.zsh

if [ "$TERM" != 'eterm-color' ]; then
    if [ -f $CONTRIB/zsh-users/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]; then
        ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets)
        source $CONTRIB/zsh-users/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
    fi
fi

# LS CONFIG
# Find the option for using colors in ls, depending on the version: Linux or BSD
if ls --color -d . &>/dev/null; then
    alias ls='ls -F --color=tty'
else
    alias ls='ls -FG'
fi

zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"

unsetopt flowcontrol
unsetopt beep
setopt auto_pushd
setopt pushd_ignore_dups
setopt interactive_comments
setopt rc_quotes
setopt short_loops
setopt no_nomatch

KEYTIMEOUT=20

# Source local config
if [ -f $HOME/.zshrc.local ]; then
    source $HOME/.zshrc.local
fi
