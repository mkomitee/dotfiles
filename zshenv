#!/usr/bin/env zsh

UNAME=$(uname)
ARCH=$(arch)

export GOROOT=$HOME/.${UNAME}.${ARCH}/go
export ERL_TOP=$HOME/.${UNAME}.${ARCH}/erlang
export ELIXIR_TOP=$HOME/.${UNAME}.${ARCH}/elixir

# Setup zsh fpath
fpath=(
    $HOME/.dotfiles/zsh
    $fpath
)
fpath=($^fpath(N))
export FPATH

# Setup path
path=(
    $HOME/bin
    $HOME/.cabal/bin
    $HOME/.cargo/bin
    $HOME/scripts
    $HOME/.${UNAME}.${ARCH}/bin
    $GOROOT/bin
    $ERL_TOP/bin
    $ELIXIR_TOP/bin
    $HOME/.dotfiles/contrib/visionmedia/git-extras/bin
    $HOME/.dotfiles/contrib/willgit/mainline/bin
    $HOME/.dotfiles/contrib/brendangregg/FlameGraph
    $HOME/.gem/ruby/1.8/bin
    $HOME/.rvm/bin
    $HOME/.pyenv/bin
    /usr/local/java/jdk/bin
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
export LESS='-g -i -M -R -S -w -X -Q -s -z-4'

export PAGER=less
# export EDITOR="emacsclient -a emacs -q -t"
export EDITOR="vim"
export SVNEDITOR=$EDITOR
export VISUAL=$EDITOR

export PYTHONDONTWRITEBYTECODE=1
export PEP8_ARGS='--ignore=E501'
export PYLINT_ARGS='-d C0301,W0142'

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

export FZF_DEFAULT_COMMAND="rg --files --hidden --follow"
export FZF_DEFAULT_OPTS="--inline-info"

if [ -f $HOME/.zshenv.local ]; then
    source $HOME/.zshenv.local
fi

unsetopt global_rcs
