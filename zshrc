#!/bin/zsh


DOT=$HOME/.dotfiles
ZSH=$DOT/zsh
LIB=$DOT/lib
CONTRIB=$DOT/contrib

fpath=($ZSH $CONTRIB/zsh-users/zsh-completions/src $fpath)

if [ -d $CONTRIB/zsh-users/zsh-completions/src ] ; then
    fpath=($fpath $CONTRIB/zsh-users/zsh-completions/src)
fi

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
source $ZSH/latex.zsh
source $ZSH/extract.zsh

if [ -d $CONTRIB/zsh-users/zsh-syntax-highlighting ]; then
    ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets)
    source $CONTRIB/zsh-users/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi

# GREP CONFIG
export GREP_OPTIONS='--color=auto --exclude-dir=.git --exclude-dir=.svn --exclude-dir=CVS --binary-files=without-match'
export GREP_COLOR='1;32'

# ACK CONFIG
export ACK_COLOR_MATCH='red'
export ACKRC=~/.dotfiles/ackrc

# LESS CONFIG
export LESS="-RMQ"

# LS CONFIG
# Find the option for using colors in ls, depending on the version: Linux or BSD
if ls --color -d . &>/dev/null; then
    alias ls='ls -F --color=tty'
else
    alias ls='ls -FG'
fi

if [ -f $HOME/.dircolors ]; then
    DIRCOLORSFILE=$HOME/.dircolors
else
    DIRCOLORSFILE=$HOME/.dotfiles/dircolors
fi

if which dircolors >/dev/null 2>&1; then
    unset LSCOLORS
    eval `dircolors $DIRCOLORSFILE`
elif which gdircolors >/dev/null 2>&1; then
    unset LSCOLORS
    eval `gdircolors $DIRCOLORSFILE`
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

ACTIVATE="$HOME/.venv/$(uname).$(arch)/bin/activate"
if [ -f $ACTIVATE ]; then
    . $ACTIVATE
fi

typeset -A abbreviations
abbreviations=(
"HEAD^"     "HEAD\\^"
"HEAD^^"    "HEAD\\^\\^"
"HEAD^^^"   "HEAD\\^\\^\\^"
"HEAD^^^^"  "HEAD\\^\\^\\^\\^\\^"
"HEAD^^^^^" "HEAD\\^\\^\\^\\^\\^"
)

KEYTIMEOUT=20
