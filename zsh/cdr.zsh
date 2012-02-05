#!/bin/zsh

autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs
zstyle ':chpwd:*' recent-dirs-file "${HOME}/.zrecent-dirs/${HOST%%.*}-${EUID}-$$-$ZSH_VERSION"

