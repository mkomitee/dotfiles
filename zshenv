#!/usr/bin/env zsh

UNAME=$(uname)
ARCH=$(arch)

export GOROOT=$HOME/.${UNAME}.${ARCH}/go

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
    $HOME/.${UNAME}.${ARCH}/bin
    $GOROOT/bin
    $HOME/.dotfiles/contrib/visionmedia/git-extras/bin
    $HOME/.dotfiles/contrib/willgit/mainline/bin
    $HOME/.rvm/bin
    $HOME/.pyenv/bin
    /usr/local/opt/coreutils/libexec/gnubin
    /usr/local/bin
    /usr/local/sbin
    /usr/bin
    /bin
    /usr/sbin
    /sbin
    /opt/X11/bin
    /usr/texbin
    /opt/bin
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
    /opt/share/man
)
manpath=($^manpath(N))
export MANPATH

# perl path, ...
perl5lib=(
    $HOME/.${UNAME}.${ARCH}/lib64/perl5
    $HOME/.${UNAME}.${ARCH}/share/perl5
)
perl5lib=($^perl5lib(N))
export PERL5LIB

# Activate the default python virtual environment
VENV="$HOME/.venv/${UNAME}.${ARCH}"
if [ -f $VENV ]; then
    source $VENV/bin/activate
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

export PYTHONDONTWRITEBYTECODE=1
export PEP8_ARGS='--ignore=E501'
export PYLINT_ARGS='-d C0301,W0142'

if [ -f $HOME/.zshenv.local ]; then
    source $HOME/.zshenv.local
fi
