## Command history configuration
HISTFILE=$HOME/.zsh_history
ZSH_UNIFIED_HISTORY=$HOME/.zsh_unified_history
HISTSIZE=10000
SAVEHIST=10000

setopt append_history
setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_find_no_dups
setopt hist_ignore_space
setopt hist_verify
setopt hist_reduce_blanks

zmodload zsh/datetime
zshaddhistory() {
    CMD=${1%%$'\n'}
    if [ ${CMD:0:1} != ' ' ]; then
        grep -Ev "^: [[:digit:]]+:[[:digit:]]+;$CMD$" $ZSH_UNIFIED_HISTORY > $ZSH_UNIFIED_HISTORY.$$ 2> /dev/null
        echo ": $EPOCHSECONDS:0;$CMD" >> $ZSH_UNIFIED_HISTORY.$$
        tail -n $HISTSIZE $ZSH_UNIFIED_HISTORY.$$ > $ZSH_UNIFIED_HISTORY
        rm $ZSH_UNIFIED_HISTORY.$$
    fi
}

function unified-history() {
    fc -ap $ZSH_UNIFIED_HISTORY
    if [ "$*" = "" ]; then
        fc -il 1
    else
        fc $*
    fi
}

function history() {
    if [ "$*" = "" ]; then
        fc -il 1
    else
        fc $*
    fi
}
