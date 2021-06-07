if [ -n "${DIDZSHENV}" ]; then
    return
fi

UNAME=$(uname)
ARCH=$(arch)

OSNAME="${UNAME}.${ARCH}"
export OSNAME

GOPATH=$HOME/.go
export GOPATH

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
    /opt/homebrew/bin
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
export EDITOR="nvim"
export SVNEDITOR=$EDITOR
export VISUAL=$EDITOR

export PYTHONDONTWRITEBYTECODE=1

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

export FZF_DEFAULT_COMMAND="rg --files --hidden --follow"
export FZF_DEFAULT_OPTS="--inline-info"
export SKIM_DEFAULT_COMMAND="fd --type f || git ls-tree -r --name-only HEAD || rg --files || find ."
export SKIM_DEFAULT_OPTS="--ansi --border --inline-info --color=dark"

if [ -f $HOME/.zshenv.local ]; then
    source $HOME/.zshenv.local
fi

unsetopt global_rcs

export DIDZSHENV=1
