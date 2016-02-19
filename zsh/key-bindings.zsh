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

# Expands .... to ../..
function expand-dot-to-parent-directory-path {
    if [[ $LBUFFER = *.. ]]; then
        LBUFFER+='/..'
    else
        LBUFFER+='.'
    fi
}
zle -N expand-dot-to-parent-directory-path

autoload edit-command-line
zle -N edit-command-line

bindkey -M vicmd "$key_info[End]" end-of-line
bindkey -M vicmd "$key_info[Home]" beginning-of-line
bindkey -M vicmd "/" history-incremental-search-forward
bindkey -M vicmd "?" history-incremental-search-backward
bindkey -M vicmd "G" end-of-history
bindkey -M vicmd "^R" vi-history-search-backward
bindkey -M vicmd "^[[A" history-search-backward
bindkey -M vicmd "^[[B" history-search-forward
bindkey -M vicmd "gg" beginning-of-history
bindkey -M vicmd "j" history-search-forward
bindkey -M vicmd "k" history-search-backward
bindkey -M vicmd v edit-command-line
bindkey -M viins "$key_info[Down]" history-search-forward
bindkey -M viins "$key_info[End]" end-of-line
bindkey -M viins "$key_info[Home]" beginning-of-line
bindkey -M viins "$key_info[Up]" history-search-backward
bindkey -M viins "." expand-dot-to-parent-directory-path
bindkey -M viins "^O" expand-cmd-path
bindkey -M viins "^R" vi-history-search-backward
bindkey -M viins "^[[A" history-search-backward
bindkey -M viins "^[[B" history-search-forward
bindkey -M viins ' ' magic-space

# These are the non-conflicting bindings from zsh emacs bindings. I've included
# them here to make life easier in the event an emacser is typing in my
# terminal. I'm a good guy like that.
bindkey -M vicmd "\M-^@"-"\M-^?" self-insert
bindkey -M vicmd "^@" set-mark-command
bindkey -M vicmd "^A" beginning-of-line
bindkey -M vicmd "^B" backward-char
bindkey -M vicmd "^E" end-of-line
bindkey -M vicmd "^F" forward-char
bindkey -M vicmd "^I" expand-or-complete
bindkey -M vicmd "^K" kill-line
bindkey -M vicmd "^O" accept-line-and-down-history
bindkey -M vicmd "^Q" push-line
bindkey -M vicmd "^S" history-incremental-search-forward
bindkey -M vicmd "^T" transpose-chars
bindkey -M vicmd "^U" kill-whole-line
bindkey -M vicmd "^V" quoted-insert
bindkey -M vicmd "^W" backward-kill-word
bindkey -M vicmd "^X*" expand-word
bindkey -M vicmd "^X=" what-cursor-position
bindkey -M vicmd "^XG" list-expand
bindkey -M vicmd "^X^B" vi-match-bracket
bindkey -M vicmd "^X^F" vi-find-next-char
bindkey -M vicmd "^X^J" vi-join
bindkey -M vicmd "^X^K" kill-buffer
bindkey -M vicmd "^X^N" infer-next-history
bindkey -M vicmd "^X^O" overwrite-mode
bindkey -M vicmd "^X^U" undo
bindkey -M vicmd "^X^V" vi-cmd-mode
bindkey -M vicmd "^X^X" exchange-point-and-mark
bindkey -M vicmd "^Xg" list-expand
bindkey -M vicmd "^Xr" history-incremental-search-backward
bindkey -M vicmd "^Xs" history-incremental-search-forward
bindkey -M vicmd "^Xu" undo
bindkey -M vicmd "^Y" yank
bindkey -M vicmd "^[!" expand-history
bindkey -M vicmd "^['" quote-line
bindkey -M vicmd "^[-" neg-argument
bindkey -M vicmd "^[." insert-last-word
bindkey -M vicmd "^[0" digit-argument
bindkey -M vicmd "^[1" digit-argument
bindkey -M vicmd "^[2" digit-argument
bindkey -M vicmd "^[3" digit-argument
bindkey -M vicmd "^[4" digit-argument
bindkey -M vicmd "^[5" digit-argument
bindkey -M vicmd "^[6" digit-argument
bindkey -M vicmd "^[7" digit-argument
bindkey -M vicmd "^[8" digit-argument
bindkey -M vicmd "^[9" digit-argument
bindkey -M vicmd "^[A" accept-and-hold
bindkey -M vicmd "^[B" backward-word
bindkey -M vicmd "^[C" capitalize-word
bindkey -M vicmd "^[D" kill-word
bindkey -M vicmd "^[F" forward-word
bindkey -M vicmd "^[G" get-line
bindkey -M vicmd "^[H" run-help
bindkey -M vicmd "^[L" down-case-word
bindkey -M vicmd "^[N" history-search-forward
bindkey -M vicmd "^[P" history-search-backward
bindkey -M vicmd "^[Q" push-line
bindkey -M vicmd "^[S" spell-word
bindkey -M vicmd "^[T" transpose-words
bindkey -M vicmd "^[U" up-case-word
bindkey -M vicmd "^[W" copy-region-as-kill
bindkey -M vicmd "^[\"" quote-region
bindkey -M vicmd "^[\$" spell-word
bindkey -M vicmd "^[^D" list-choices
bindkey -M vicmd "^[^G" send-break
bindkey -M vicmd "^[^H" backward-kill-word
bindkey -M vicmd "^[^I" self-insert-unmeta
bindkey -M vicmd "^[^J" self-insert-unmeta
bindkey -M vicmd "^[^L" clear-screen
bindkey -M vicmd "^[^M" self-insert-unmeta
bindkey -M vicmd "^[a" accept-and-hold
bindkey -M vicmd "^[b" backward-word
bindkey -M vicmd "^[c" capitalize-word
bindkey -M vicmd "^[d" kill-word
bindkey -M vicmd "^[f" forward-word
bindkey -M vicmd "^[g" get-line
bindkey -M vicmd "^[h" run-help
bindkey -M vicmd "^[l" down-case-word
bindkey -M vicmd "^[n" history-search-forward
bindkey -M vicmd "^[p" history-search-backward
bindkey -M vicmd "^[q" push-line
bindkey -M vicmd "^[s" spell-word
bindkey -M vicmd "^[t" transpose-words
bindkey -M vicmd "^[u" up-case-word
bindkey -M vicmd "^[w" copy-region-as-kill
bindkey -M vicmd "^[x" execute-named-cmd
bindkey -M vicmd "^[y" yank-pop
bindkey -M vicmd "^[z" execute-last-named-cmd
bindkey -M viins "^@" set-mark-command
bindkey -M viins "^A" beginning-of-line
bindkey -M viins "^B" backward-char
bindkey -M viins "^E" end-of-line
bindkey -M viins "^F" forward-char
bindkey -M viins "^N" down-line-or-history
bindkey -M viins "^P" up-line-or-history
bindkey -M viins "^S" history-incremental-search-forward
bindkey -M viins "^T" transpose-chars
bindkey -M viins "^X*" expand-word
bindkey -M viins "^X=" what-cursor-position
bindkey -M viins "^XG" list-expand
bindkey -M viins "^X^B" vi-match-bracket
bindkey -M viins "^X^F" vi-find-next-char
bindkey -M viins "^X^J" vi-join
bindkey -M viins "^X^K" kill-buffer
bindkey -M viins "^X^N" infer-next-history
bindkey -M viins "^X^O" overwrite-mode
bindkey -M viins "^X^U" undo
bindkey -M viins "^X^V" vi-cmd-mode
bindkey -M viins "^X^X" exchange-point-and-mark
bindkey -M viins "^Xg" list-expand
bindkey -M viins "^Xr" history-incremental-search-backward
bindkey -M viins "^Xs" history-incremental-search-forward
bindkey -M viins "^Xu" undo
bindkey -M viins "^Y" yank
bindkey -M viins "^[!" expand-history
bindkey -M viins "^['" quote-line
bindkey -M viins "^[-" neg-argument
bindkey -M viins "^[." insert-last-word
bindkey -M viins "^[0" digit-argument
bindkey -M viins "^[1" digit-argument
bindkey -M viins "^[2" digit-argument
bindkey -M viins "^[3" digit-argument
bindkey -M viins "^[4" digit-argument
bindkey -M viins "^[5" digit-argument
bindkey -M viins "^[6" digit-argument
bindkey -M viins "^[7" digit-argument
bindkey -M viins "^[8" digit-argument
bindkey -M viins "^[9" digit-argument
bindkey -M viins "^[A" accept-and-hold
bindkey -M viins "^[B" backward-word
bindkey -M viins "^[C" capitalize-word
bindkey -M viins "^[D" kill-word
bindkey -M viins "^[F" forward-word
bindkey -M viins "^[G" get-line
bindkey -M viins "^[H" run-help
bindkey -M viins "^[L" down-case-word
bindkey -M viins "^[N" history-search-forward
bindkey -M viins "^[P" history-search-backward
bindkey -M viins "^[Q" push-line
bindkey -M viins "^[S" spell-word
bindkey -M viins "^[T" transpose-words
bindkey -M viins "^[U" up-case-word
bindkey -M viins "^[W" copy-region-as-kill
bindkey -M viins "^[\"" quote-region
bindkey -M viins "^[\$" spell-word
bindkey -M viins "^[^D" list-choices
bindkey -M viins "^[^G" send-break
bindkey -M viins "^[^H" backward-kill-word
bindkey -M viins "^[^I" self-insert-unmeta
bindkey -M viins "^[^J" self-insert-unmeta
bindkey -M viins "^[^L" clear-screen
bindkey -M viins "^[^M" self-insert-unmeta
bindkey -M viins "^[a" accept-and-hold
bindkey -M viins "^[b" backward-word
bindkey -M viins "^[c" capitalize-word
bindkey -M viins "^[d" kill-word
bindkey -M viins "^[f" forward-word
bindkey -M viins "^[g" get-line
bindkey -M viins "^[h" run-help
bindkey -M viins "^[l" down-case-word
bindkey -M viins "^[n" history-search-forward
bindkey -M viins "^[p" history-search-backward
bindkey -M viins "^[q" push-line
bindkey -M viins "^[s" spell-word
bindkey -M viins "^[t" transpose-words
bindkey -M viins "^[u" up-case-word
bindkey -M viins "^[w" copy-region-as-kill
bindkey -M viins "^[x" execute-named-cmd
bindkey -M viins "^[y" yank-pop
bindkey -M viins "^[z" execute-last-named-cmd
