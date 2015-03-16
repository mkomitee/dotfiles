(req-package git-gutter
  :diminish git-gutter-mode
  :config (global-git-gutter-mode)
  )

(req-package gitconfig-mode
  :config (add-to-list 'auto-mode-alist '("gitconfig\\'" . gitconfig-mode))
  )

(req-package git-gutter-fringe
  :require (evil evil-leader git-gutter)
  :config (progn
            (evil-leader/set-key
              "ga" 'git-gutter:stage-hunk
              "gr" 'git-gutter:revert-hunk
              )
            (evil-define-key 'motion global-map
              "]h" 'git-gutter:next-hunk
              "[h" 'git-gutter:previous-hunk
              )
            )
  )

(req-package magit
  :require (evil evil-leader)
  :config (progn
            (magit-auto-revert-mode -1)
            (evil-leader/set-key
              "gs" 'magit-status

              "gl" 'magit-log
              "gL" 'magit-reflog

              "gc" 'magit-commit

              "gd" 'magit-diff-unstaged
              "gD" 'magit-diff-staged
              "gb" 'magit-blame-mode
              )
            (define-key magit-status-mode-map "j" 'magit-goto-next-section)
            (define-key magit-status-mode-map "k" 'magit-goto-previous-section)
            (define-key magit-log-mode-map "j" 'magit-goto-next-section)
            (define-key magit-log-mode-map "k" 'magit-goto-previous-section)
            (define-key magit-diff-mode-map "j" 'magit-goto-next-section)
            (define-key magit-diff-mode-map "k" 'magit-goto-previous-section)
            (setq vc-follow-symlinks nil)
            )
  )

(req-package git-rebase-mode
  :require (evil evil-leader)
  :config (progn
            (evil-set-initial-state 'git-rebase-mode 'motion)
            (evil-leader/set-key-for-mode 'git-rebase-mode
              "gw" 'git-rebase-server-edit
              "gq" 'git-rebase-abort
              "ga" 'git-rebase-abort

              "gx" 'git-rebase-exec
              "gp" 'git-rebase-pick
              "gr" 'git-rebase-reword
              "ge" 'git-rebase-edit
              "gs" 'git-rebase-squash
              "gf" 'git-rebase-fixup
              "gK" 'git-rebase-kill-line

              "gj" 'git-rebase-move-line-down
              "gk" 'git-rebase-move-line-up
              )
            )
  )

(req-package git-commit-mode
  :require (evil evil-leader)
  :config (progn
            (evil-set-initial-state 'git-commit-mode 'normal)
            (evil-leader/set-key-for-mode 'git-commit-mode
              "gw" 'git-commit-commit
              "gq" 'git-commit-abort
              "ga" 'git-commit-abort
              )
            )
  )


(req-package git-timemachine
  :require (evil-leader)
  :config (progn
            (evil-leader/set-key
              "gt" 'git-timemachine
              "gT" 'git-timemachine-quit)

            (evil-define-key 'motion global-map
              "]c" 'git-timemachine-show-next-revision
              "[c" 'git-timemachine-show-previous-revision
              )
            )
  )
