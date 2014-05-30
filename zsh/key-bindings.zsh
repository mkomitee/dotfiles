function zle-line-init zle-keymap-select {
  zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select

bindkey -v
# bindkey -e

# In vi-mode, map v to edit the command line
autoload edit-command-line
zle -N edit-command-line
bindkey -M vicmd v edit-command-line

# Yes, we're in vi-mode, but ^r is a habit
bindkey -M viins "^r" vi-history-search-backward
bindkey -M vicmd "^r" vi-history-search-backward

# Sometimes its useful to find in insert mode
bindkey -M viins "^F" vi-find-next-char
bindkey -M viins "^P" vi-find-prev-char

bindkey -M vicmd "gg" beginning-of-history
bindkey -M vicmd "G" end-of-history

bindkey -M vicmd "k" history-search-backward
bindkey -M vicmd "j" history-search-forward

bindkey -M vicmd "?" history-incremental-search-backward
bindkey -M vicmd "/" history-incremental-search-forward

bindkey -M viins "^L" clear-screen
bindkey -M viins "^W" backward-kill-word
bindkey -M viins "^A" beginning-of-line
bindkey -M viins "^E" end-of-line

bindkey -M viins "^O" expand-cmd-path

bindkey -M viins "\e[A" history-search-backward
bindkey -M viins "\e[B" history-search-forward
bindkey -M vicmd "\e[A" history-search-backward
bindkey -M vicmd "\e[B" history-search-forward

bindkey -M viins 'jj' vi-cmd-mode
