#!/usr/bin/env zsh

UNAME=$(uname)
arch=$(arch)

# Setup zsh fpath
fpath=(
    $HOME/.dotfiles/zsh
    $HOME/.dotfiles/contrib//zsh-users/zsh-completions/src
    $fpath
)
fpath=($^fpath(N))
export FPATH

# Setup path
path=(
    $HOME/bin
    $HOME/.cabal/bin
    $HOME/scripts
    $HOME/.dotfiles/contrib/visionmedia/git-extras/bin
    $HOME/.dotfiles/contrib/willgit/mainline/bin
    $HOME/.rvm/bin
    /usr/local/opt/coreutils/libexec/gnubin
    /usr/local/bin
    /usr/local/sbin
    /usr/bin
    /bin
    /usr/sbin
    /sbin
    /opt/X11/bin
    /usr/texbin
    /usr/local/share/python
)
path=($^path(N))
export PATH

# Aaaand manpath
manpath=(
    $HOME/share/man
    $HOME/.cabal/share/man
    $HOME/.dotfiles/contrib/visionmedia/git-extras/man
    $HOME/.rvm/man
    /usr/local/opt/coreutils/libexec/gnuman
    /usr/local/share/man
    /usr/local/man
    /usr/share/man
    /opt/X11/share/man
    /usr/texbin/man
)
manpath=($^manpath(N))
export MANPATH

# Activate the default python virtual environment
VENV="$HOME/.venv/${UNAME}.${ARCH}"
if [ -f $VENV ]; then
    . $VENV/bin/activate
fi

# GREP CONFIG
export GREP_OPTIONS='--color=auto --exclude-dir=.git --exclude-dir=.svn --exclude-dir=CVS --binary-files=without-match'
export GREP_COLOR='1;32'

# ACK CONFIG
export ACK_COLOR_MATCH='red'
export ACKRC=~/.dotfiles/ackrc

# LESS CONFIG
export LESS="-RMQ"

export PAGER=less
export EDITOR=vim
export SVNEDITOR=$EDITOR
export VISUAL=$EDITOR

if [ -f $HOME/.zshenv.local ]; then
    source $HOME/.zshenv.local
fi
