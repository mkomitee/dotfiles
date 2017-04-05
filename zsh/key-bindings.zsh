#!/usr/bin/env zsh

if [ "$TERM" = "dumb" ]; then
    return
fi

bindkey -v

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


autoload edit-command-line
zle -N edit-command-line

zsh-widget-noop () {}
zle -N zsh-widget-noop

# pressing <ESC> in normal mode is bogus: you need to press 'i' twice to enter insert mode again.
# rebinding <ESC> in normal mode to something harmless solves the problem.
bindkey -M vicmd '\e' zsh-widget-noop

bindkey -M vicmd "$key_info[End]" end-of-line
bindkey -M vicmd "$key_info[Home]" beginning-of-line
bindkey -M vicmd "?" history-incremental-search-backward
bindkey -M vicmd "/" history-incremental-search-forward
bindkey -M vicmd "G" end-of-history
bindkey -M vicmd "^R" history-incremental-search-backward
bindkey -M vicmd "gg" beginning-of-history
bindkey -M vicmd v edit-command-line
bindkey -M viins "$key_info[Down]" history-search-forward
bindkey -M viins "$key_info[End]" end-of-line
bindkey -M viins "$key_info[Home]" beginning-of-line
bindkey -M viins "$key_info[Up]" history-search-backward
bindkey -M viins "^O" expand-cmd-path
bindkey -M viins "^R" history-incremental-search-backward
bindkey -M viins ' ' magic-space

# vi-backward-delete-char does not go back across newlines.
bindkey -M viins "^H" backward-delete-char
bindkey -M viins "^?" backward-delete-char


zsh-widget-tmux-pane-left () {
  if [ ! -z "$TMUX" ]; then
    tmux select-pane -L
  fi
}
zle -N zsh-widget-tmux-pane-left

zsh-widget-tmux-pane-down () {
  if [ ! -z "$TMUX" ]; then
    tmux select-pane -D
  fi
}
zle -N zsh-widget-tmux-pane-down

zsh-widget-tmux-pane-up () {
  if [ ! -z "$TMUX" ]; then
    tmux select-pane -U
  fi
}
zle -N zsh-widget-tmux-pane-up

zsh-widget-tmux-pane-right () {
  if [ ! -z "$TMUX" ]; then
    tmux select-pane -R
  fi
}
zle -N zsh-widget-tmux-pane-right

zsh-widget-tmux-pane-vsplit () {
  if [ ! -z "$TMUX" ]; then
    tmux split
  fi
}
zle -N zsh-widget-tmux-pane-vsplit

zsh-widget-tmux-pane-hsplit () {
  if [ ! -z "$TMUX" ]; then
    tmux split -h
  fi
}
zle -N zsh-widget-tmux-pane-hsplit

zsh-widget-tmux-pane-delete () {
  if [ ! -z "$TMUX" ]; then
    tmux kill-pane
  fi
}
zle -N zsh-widget-tmux-pane-hsplit


bindkey -M vicmd -r " "
bindkey -M vicmd " wh" zsh-widget-tmux-pane-left
bindkey -M vicmd " wj" zsh-widget-tmux-pane-down
bindkey -M vicmd " wk" zsh-widget-tmux-pane-up
bindkey -M vicmd " wl" zsh-widget-tmux-pane-right
bindkey -M vicmd " w-" zsh-widget-tmux-pane-vsplit
bindkey -M vicmd " w/" zsh-widget-tmux-pane-hsplit
bindkey -M vicmd " wd" zsh-widget-tmux-pane-delete
