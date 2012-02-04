#!/bin/zsh
#
ZSH=$HOME/.dotfiles/zsh
fpath=($ZSH $fpath)

source $ZSH/pathrc.zsh
source $ZSH/termsupport.zsh
source $ZSH/completion.zsh
source $ZSH/history.zsh
source $ZSH/aliases.zsh
source $ZSH/key-bindings.zsh
source $ZSH/functions.zsh
source $ZSH/prompt.zsh

# GREP CONFIG
export GREP_OPTIONS='--color=auto'
export GREP_COLOR='1;32'

# LS CONFIG
# Find the option for using colors in ls, depending on the version: Linux or BSD
ls --color -d . &>/dev/null 2>&1 && alias ls='ls --color=tty' || alias ls='ls -G'
export LSCOLORS="Gxfxcxdxbxegedabagacad"

export PAGER=less

# Source local config
if [ -f $HOME/.zshrc.local ]; then
    source $HOME/.zshrc.local
fi
