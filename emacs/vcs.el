(req-package git-gutter
  :diminish git-gutter-mode
  :config (global-git-gutter-mode)
  )

(req-package git-gutter-fringe
  :require evil evil-leader git-gutter-mode
  :config (progn
            (evil-leader/set-key
              "gha" 'git-gutter:stage-hunk
              "ghs" 'git-gutter:stage-hunk
              "ghr" 'git-gutter:revert-hunk
              "ghN" 'git-gutter:previous-hunk
              "ghn" 'git-gutter:next-hunk
              )
            (evil-define-key 'motion global-map
              "]h" 'git-gutter:next-hunk
              "[h" 'git-gutter:previous-hunk
              )
            )
  )

(req-package magit
  :require evil evil-leader
  :config (progn
            (magit-auto-revert-mode -1)
            (evil-leader/set-key
              "gs" 'magit-status
              "gll" 'magit-log
              "glr" 'magit-reflog
              "gc" 'magit-commit
              "gP" 'magit-push
              "gD" 'magit-diff-unstaged
              "gdh" 'magit-diff-unstaged
              "gds" 'magit-diff-staged
              )

            (evil-define-key 'motion magit-mode-map
              "j" 'magit-goto-next-section
              "k" 'magit-goto-previous-section
              "J" 'magit-goto-next-sibling-section
              "K" 'magit-goto-previous-sibling-section
              "H" 'magit-goto-parent-section
              )

            (evil-set-initial-state 'magit-status-mode 'motion)
            (evil-define-key 'motion magit-status-mode-map
              "q" 'magit-mode-quit-window
              (kbd "RET") 'magit-visit-item
              "s" 'magit-stage-item
              "u" 'magit-unstage-item
              "i" 'magit-ignore-item
              "I" 'magit-ignore-item-locally
              "J" 'magit-section-jump-map
              "." 'magit-mark-item
              "=" 'magit-diff-with-mark
              "d" 'magit-ediff
              "K" 'magit-discard-item
              "g" 'magit-refresh
              "G" 'magit-refresh-all
              "c" 'magit-commit
              "C" 'magit-commit-add-log
              "m" 'magit-merge
              "f" 'magit-fetch-current
              "F" 'magit-pull
              "!" 'magit-git-command-topdir
              "P" 'magit-push
              "t" 'magit-tag
              "l" 'magit-log
              "z" 'magit-stash
              "r" 'magit-interactive-rebase
              )

            (evil-leader/set-key-for-mode 'magit-status-mode
              ";q" 'magit-mode-quit-window
              ";s" 'magit-stage-item
              ";u" 'magit-unstage-item
              ";i" 'magit-ignore-item
              ";I" 'magit-ignore-item-locally
              ";j" 'magit-section-jump-map
              ";." 'magit-mark-item
              ";=" 'magit-diff-with-mark
              ";d" 'magit-ediff
              ";k" 'magit-discard-item
              ";g" 'magit-refresh
              ";G" 'magit-refresh-all
              ";c" 'magit-commit
              ";C" 'magit-commit-add-log
              ";m" 'magit-merge
              ";f" 'magit-fetch-current
              ";F" 'magit-pull
              ";!" 'magit-git-command-topdir
              ";P" 'magit-push
              ";t" 'magit-tag
              ";l" 'magit-log
              ";z" 'magit-stash
              ";r" 'magit-interactive-rebase
              )

            (evil-set-initial-state 'magit-diff-mode 'motion)
            (evil-define-key 'motion magit-diff-mode-map
              "q" 'magit-mode-quit-window
              )
            (evil-leader/set-key-for-mode 'magit-diff-mode
              ";q" 'magit-mode-quit-window
              )

            (evil-set-initial-state 'magit-cherry-mode 'emacs)
            (evil-define-key 'motion magit-cherry-mode-map
              "q" 'magit-mode-quit-window
              )
            (evil-leader/set-key-for-mode 'magit-cherry-mode
              ";q" 'magit-mode-quit-window
              )

            (evil-set-initial-state 'magit-log-mode 'motion)
            (evil-define-key 'motion magit-log-mode-map
              "q" 'magit-mode-quit-window
              )
            (evil-leader/set-key-for-mode 'magit-log-mode
              ";q" 'magit-mode-quit-window
              ";e" 'magit-log-show-more-entries
              ";h" 'magit-log-toggle-margin
              ";=" 'magit-diff-with-mark
              ";d" 'magit-ediff
              ";g" 'magit-refresh
              ";G" 'magit-refresh-all
              )

            (evil-set-initial-state 'magit-reflog-mode 'motion)
            (evil-define-key 'motion magit-reflog-mode-map
              "q" 'magit-mode-quit-window
              )
            (evil-leader/set-key-for-mode 'magit-reflog-mode
              ";q" 'magit-mode-quit-window
              ";e" 'magit-log-show-more-entries
              ";h" 'magit-log-toggle-margin
              ";=" 'magit-diff-with-mark
              ";d" 'magit-ediff
              ";g" 'magit-refresh
              ";G" 'magit-refresh-all
              )

            (evil-ex-define-cmd "log" 'magit-log)
            (evil-set-initial-state 'magit-key-mode 'emacs)
            )
  )

(req-package git-rebase-mode
  :require evil evil-leader
  :config (progn
            (evil-set-initial-state 'git-rebase-mode 'motion)
            (evil-leader/set-key-for-mode 'git-rebase-mode
              ";w" 'git-rebase-server-edit
              ";q" 'git-rebase-abort
              ";a" 'git-rebase-abort
              ";x" 'git-rebase-exec
              ";p" 'git-rebase-pick
              ";r" 'git-rebase-reword
              ";e" 'git-rebase-edit
              ";s" 'git-rebase-squash
              ";f" 'git-rebase-fixup
              ";D" 'git-rebase-kill-line
              ";P" 'git-rebase-insert
              ";J" 'git-rebase-move-line-down
              ";K" 'git-rebase-move-line-up
              )
            )
  )

(req-package git-commit-mode
  :require evil evil-leader
  :config (progn
            (evil-set-initial-state 'git-commit-mode 'normal)
            (evil-leader/set-key-for-mode 'git-commit-mode
              ";w" 'git-commit-commit
              ";q" 'git-commit-abort
              ";a" 'git-commit-abort
              )
            )
  )

