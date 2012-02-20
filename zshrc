#!/bin/zsh

ZSH=$HOME/.dotfiles/zsh
fpath=($ZSH $fpath)

export PYTHONPATH=$HOME/lib/python:$HOME/.dotfiles/lib/python:$PYTHONPATH

source $ZSH/pathrc.zsh
source $ZSH/cdr.zsh
source $ZSH/rvm.zsh
source $ZSH/termsupport.zsh
source $ZSH/lang.zsh
source $ZSH/completion.zsh
source $ZSH/history.zsh
source $ZSH/aliases.zsh
source $ZSH/key-bindings.zsh
source $ZSH/globbing.zsh
source $ZSH/functions.zsh
source $ZSH/jobs.zsh
source $ZSH/virtualenv.zsh
source $ZSH/prompt.zsh

# GREP CONFIG
export GREP_OPTIONS='--color=auto'
export GREP_COLOR='1;32'

# LS CONFIG
# Find the option for using colors in ls, depending on the version: Linux or BSD
ls --color -d . &>/dev/null 2>&1 && alias ls='ls --color=tty' || alias ls='ls -G'
export LSCOLORS="Gxfxcxdxbxegedabagacad"

export PAGER=less
export EDITOR=vim
export SVNEDITOR=vim
export VISUAL=vim

unsetopt flowcontrol
unsetopt beep
setopt auto_cd
setopt auto_pushd
setopt pushd_ignore_dups
setopt interactive_comments
setopt rc_quotes
setopt short_loops

# Source local config
if [ -f $HOME/.zshrc.local ]; then
    source $HOME/.zshrc.local
fi
