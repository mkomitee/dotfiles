if [ -n "${DIDZSHENV}" ]; then
    return
fi

UNAME=$(uname)
ARCH=$(arch)

OSNAME="${UNAME}.${ARCH}"
export OSNAME

# Setup zsh fpath
fpath=(
    $HOME/.dotfiles/zsh
    $fpath
)
fpath=($^fpath(N))
export FPATH

# Setup path
path=(
    $HOME/.local/bin
    $HOME/bin
    $HOME/.cargo/bin
    $HOME/.go/bin
    $HOME/scripts
    $HOME/.${OSNAME}/bin
    /usr/local/bin
    /usr/local/sbin
    /usr/bin
    /bin
    /usr/sbin
    /sbin
)
path=($^path(N))
export PATH

export PAGER=less
export EDITOR="vim"
export SVNEDITOR=$EDITOR
export VISUAL=$EDITOR

export PYTHONDONTWRITEBYTECODE=1

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

export FZF_DEFAULT_COMMAND="rg --files --hidden --follow"
export FZF_DEFAULT_OPTS="--inline-info"

if [ -f $HOME/.zshenv.local ]; then
    source $HOME/.zshenv.local
fi

unsetopt global_rcs

export DIDZSHENV=1
