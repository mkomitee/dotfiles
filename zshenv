#!/usr/bin/env zsh

UNAME=$(uname)
arch=$(arch)

# Setup zsh fpath
fpath=(
    $HOME/.dotfiles/zsh
    $HOME/.dotfiles/contrib//zsh-users/zsh-completions/src
    $fpath
)

# Filter out any fpath directories which don't exist
fpath=($^fpath(N))

# Setup path
path=(
    $HOME/bin
    $HOME/.cabal/bin
    $HOME/scripts
    $HOME/.dotfiles/contrib/visionmedia/git-extras/bin
    $HOME/.dotfiles/contrib/willgit/mainline/bin
    $HOME/.rvm/bin
    /usr/local/bin
    /usr/local/sbin
    /usr/bin
    /bin
    /usr/sbin
    /sbin
    /opt/X11/bin
    /usr/texbin
    # $path
)

# Prefer homebrew coreutils if availale
coreutils=$(brew --prefix coreutils) 2> /dev/null
if [ -n $coreutils ]; then
    path=(
        $coreutils/libexec/gnubin
        $path
    )
fi

# Filter out any path directories which don't exist
path=($^path(N))

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
