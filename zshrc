#!/bin/zsh

ZSH=$HOME/.dotfiles/zsh
LIB=$HOME/.dotfiles/lib

fpath=($ZSH $fpath)

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
source $ZSH/prompt.zsh
source $ZSH/colours.zsh
source $ZSH/funny.zsh

# GREP CONFIG
export GREP_OPTIONS='--color=auto'
export GREP_COLOR='1;32'

# LS CONFIG
# Find the option for using colors in ls, depending on the version: Linux or BSD
if ls --color -d . &>/dev/null; then
    alias ls='ls -F --color=tty --group-directories-first'
else
    alias ls='ls -FG'
fi
if which dircolors >/dev/null 2>&1; then
    unset LSCOLORS
    eval `dircolors $HOME/.dotfiles/dircolors`
elif which gdircolors >/dev/null 2>&1; then
    unset LSCOLORS
    eval `gdircolors $HOME/.dotfiles/dircolors`
fi
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"


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
