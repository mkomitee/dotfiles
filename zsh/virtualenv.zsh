#!/bin/zsh

export VENV_ROOT="$HOME/.virtualenvs"
function venv() {
    source $VENV_ROOT/$1/bin/activate
}
