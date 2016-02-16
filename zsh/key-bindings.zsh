#!/usr/bin/env zsh

if [ "$TERM" = "dumb" ]; then
    return
fi
# Use human-friendly identifiers.
zmodload zsh/terminfo
typeset -gA key_info
key_info=(
  'Control'      '\C-'
  'ControlLeft'  '\e[1;5D \e[5D \e\e[D \eOd'
  'ControlRight' '\e[1;5C \e[5C \e\e[C \eOc'
  'Escape'       '\e'
  'Meta'         '\M-'
  'Backspace'    "^?"
  'Delete'       "^[[3~"
  'F1'           "$terminfo[kf1]"
  'F2'           "$terminfo[kf2]"
  'F3'           "$terminfo[kf3]"
  'F4'           "$terminfo[kf4]"
  'F5'           "$terminfo[kf5]"
  'F6'           "$terminfo[kf6]"
  'F7'           "$terminfo[kf7]"
  'F8'           "$terminfo[kf8]"
  'F9'           "$terminfo[kf9]"
  'F10'          "$terminfo[kf10]"
  'F11'          "$terminfo[kf11]"
  'F12'          "$terminfo[kf12]"
  'Insert'       "$terminfo[kich1]"
  'Home'         "$terminfo[khome]"
  'PageUp'       "$terminfo[kpp]"
  'End'          "$terminfo[kend]"
  'PageDown'     "$terminfo[knp]"
  'Up'           "$terminfo[kcuu1]"
  'Left'         "$terminfo[kcub1]"
  'Down'         "$terminfo[kcud1]"
  'Right'        "$terminfo[kcuf1]"
  'BackTab'      "$terminfo[kcbt]"
)



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

bindkey ' ' magic-space

# Expands .... to ../..
function expand-dot-to-parent-directory-path {
    if [[ $LBUFFER = *.. ]]; then
        LBUFFER+='/..'
    else
        LBUFFER+='.'
    fi
}
zle -N expand-dot-to-parent-directory-path
bindkey -M "viins" "." expand-dot-to-parent-directory-path
bindkey -M "viins" "$key_info[Home]" beginning-of-line
bindkey -M "viins" "$key_info[End]" end-of-line
bindkey -M "vicmd" "$key_info[Home]" beginning-of-line
bindkey -M "vicmd" "$key_info[End]" end-of-line
