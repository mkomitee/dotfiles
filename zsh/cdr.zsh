#!/bin/zsh


if [[ $ZSH_VERSION == 4.3.<11->* ||
      $ZSH_VERSION == 4.<4->* ||
      $ZSH_VERSION == <5->* ]]; then
    autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
    add-zsh-hook chpwd chpwd_recent_dirs
    zstyle ':chpwd:*' recent-dirs-file "${HOME}/.zrecent-dirs/${HOST%%.*}-${EUID}-$$-$ZSH_VERSION"
    alias dirs='cdr -l'
else
    setopt auto_pushd
    setopt pushd_ignore_dups
    setopt pushd_silent
    alias dirs='dirs -pvl'
    alias cdr='popd'
fi

