## Command history configuration
HISTFILE=$HOME/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

setopt append_history
setopt extended_history
setopt hist_expire_dups_first
setopt hist_find_no_dups
setopt hist_ignore_all_dups
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_reduce_blanks
setopt hist_verify
setopt inc_append_history

function hgrep() {
    fc -ap $HISTFILE
    noglob fc -lnm *"$*"* 1
}

alias hgrep="noglob hgrep"

function history() {
    noglob fc -lm *"$*"* 1
}

alias history="noglob history"
