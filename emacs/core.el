(setq auto-save-list-file-prefix "~/.emacs.d/.auto-save-list/.saves-"
      inhibit-startup-echo-area-message t
      inhibit-startup-screen t
      initial-buffer-choice (quote remember-notes)
      frame-background-mode (quote dark)
      indicate-buffer-boundaries (quote left)
      scroll-conservatively 9999
      scroll-margin 3
      scroll-preserve-screen-position 1
      sentence-end-double-space nil
      tab-width 4
      ediff-split-window-function 'split-window-horizontally
      custom-file (concat komitee/emacs-config-directory "custom.el")
      )

(req-package better-defaults)

(req-package remember
  :config (setq remember-notes-initial-major-mode 'rst-mode)
  )

(req-package uniquify
  :config (setq uniquify-ignore-buffers-re "^\\*"
                uniquify-min-dir-content 1
                uniquify-trailing-separator-p t
                )
  )

;; minibuffer history
(req-package savehist
  :config (progn
            (setq savehist-additional-variables (quote (search ring regexp-search-ring))
                  savehist-autosave-interval 60
                  savehist-file "~/.emacs.d/.savehist"
                  )
            (savehist-mode +1)
            )
  )

;; recent files
(req-package recentf
  :config (progn
            (setq recentf-max-menu-items 50
                  recentf-max-saved-items 100
                  recentf-save-file "~/.emacs.d/.recentf"
                  )
            (recentf-mode +1)
            )
  )

(req-package files
  :config (setq auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/.backups/" t)))
                backup-by-copying t
                backup-directory-alist (quote (("." . "~/.emacs.d/backups")))
                create-lockfiles nil
                )
  )

(req-package eshell
  :config (setq eshell-aliases-file (concat komitee/emacs-config-directory "aliases")
                eshell-directory-name "~/"
                )
  )

;; interatively do things ...
(req-package ido
  :config (progn
            (setq ido-save-directory-list-file "~/.emacs.d/.ido.last"
                  ido-use-filename-at-point (quote guess)
                  ido-ignore-buffers (quote ("\\` "
                                             "\\*Python\\[.*\\]\\*"))


                  )
            (ido-everywhere t)
            (req-package flx-ido
              :config (flx-ido-mode t)
              )
            (req-package ido-hacks)
            (req-package ido-ubiquitous
              :config (ido-ubiquitous-mode t)
              )
            (req-package ido-vertical-mode
              :config (ido-vertical-mode)
              )
            )
  )

(defalias 'yes-or-no-p 'y-or-n-p)
(xterm-mouse-mode t)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(global-visual-line-mode)
(diminish 'visual-line-mode)
(blink-cursor-mode -1)

(require 'undo-tree)
(setq undo-tree-auto-save-history t
      undo-tree-history-directory-alist (quote (("." . "~/.emacs.d/.undo")))
      )
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

;; make sure $PATH is set correctly
(req-package exec-path-from-shell
  :config (ignore-errors ;; windows
            (exec-path-from-shell-initialize))
  )

;; Use popwin to keep control of my windows
(req-package popwin
  :config (progn
            (popwin-mode 1)
            (push '("*Flycheck errors*" :position bottom :height 5)
                  popwin:special-display-config)
            )
  )

; Emacs now has a good editor.
(setq evil-emacs-state-cursor '("red" box)
      evil-normal-state-cursor '("white" box)
      evil-insert-state-cursor '("white" bar)
      evil-backspace-join-lines t
      evil-leader/leader "<SPC>"
      evil-magic (quote very-magic)
      evil-search-module (quote evil-search)
      evil-want-C-u-scroll t
      evil-want-C-w-in-emacs-state t
      evil-want-fine-undo nil
      )

(evil-mode 1)

(req-package evil-commentary
  :diminish evil-commentary-mode
  :config (evil-commentary-default-setup)
  )

(req-package evil-easymotion
  :config (evilem-default-keybindings "M-SPC")
  )

(req-package evil-indent-textobject)

(req-package evil-jumper
  :config (global-evil-jumper-mode)
  )

(req-package evil-matchit
  :config (global-evil-matchit-mode 1)
  )

(req-package evil-snipe
  :config (global-evil-snipe-mode 1)
  )

(req-package evil-surround
  :config (global-evil-surround-mode 1)
  )

(req-package evil-tabs
  :config (global-evil-tabs-mode)
  )

(req-package evil-visualstar)

(req-package evil-numbers
  :config (progn
            (define-key evil-motion-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
            (define-key evil-motion-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)
            )
  )

(req-package evil-exchange
  :config (evil-exchange-install))


;; Update modes, everything that defaults to emacs state should
;; instead default to motion state. Anything that requires editing,
;; we'll remove from motion state so it's in notmal mode by default,
;; unless there's a VERY good reason to use emacs state.
(setq evil-motion-state-modes (append evil-emacs-state-modes
                                      evil-motion-state-modes)
      evil-emacs-state-modes nil
      evil-motion-state-modes (remove 'Custom-mode evil-motion-state-modes)
      )

;; We want _ to be considered a word character, like it is in vim.
(modify-syntax-entry ?_ "w")

;; Projectile for better fuzzy matching and more
(req-package projectile
  :require evil ag
  :config (progn
            (setq projectile-cache-file "~/.emacs.d/.projectile.cache"
                  projectile-known-projects-file "~/.emacs.d/.projectile-bookmarks.eld"
                  projectile-require-project-root nil)
            (req-package ag
              :bind ("C-c /" . ag-regexp-project-at-point)
              :config (progn
                        (setq ag-highlight-search t)
                        ;; This shouldn't be necessary, but adding
                        ;; ag-mode to evil-motion-state-modes doesn't
                        ;; seem to have the desired effect.
                        (add-hook 'ag-mode-hook 'evil-motion-state)
                        )
              )
            (projectile-global-mode t)
            )
  )

;; This shouldn't be necessary, but adding help-mode to
;; evil-motion-state-modes doesn't seem to have the desired effect.
(add-hook 'help-mode-hook 'evil-motion-state)

;; Snippets are useful
(req-package yasnippet
  :diminish yas-minor-mode
  :config (yas-global-mode 1)
  )

(req-package ace-window
  :require evil
  :config (define-key evil-window-map "?" 'ace-window)
  )

(req-package discover-my-major
  :require evil-leader
  :config (evil-leader/set-key
            "hM" 'discover-my-major
            )
  )

(req-package git-gutter-fringe
  :diminish git-gutter-mode
  :config (progn
            (global-git-gutter-mode)
            (evil-leader/set-key
              "gha" 'git-gutter:stage-hunk
              "ghs" 'git-gutter:stage-hunk
              "ghr" 'git-gutter:revert-hunk
              "ghN" 'git-gutter:previous-hunk
              "ghn" 'git-gutter:next-hunk
              )
            (define-key evil-motion-state-map "]h" 'git-gutter:next-hunk)
            (define-key evil-motion-state-map "[h" 'git-gutter:previous-hunk)
            )
  )

(req-package magit
  :require evil
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
              "j" 'magit-goto-next-section)
            (evil-define-key 'motion magit-mode-map
              "k" 'magit-goto-previous-section)
            (evil-define-key 'motion magit-mode-map
              "J" 'magit-goto-next-sibling-section)
            (evil-define-key 'motion magit-mode-map
              "K" 'magit-goto-previous-sibling-section)
            (evil-define-key 'motion magit-mode-map
              "H" 'magit-goto-parent-section)

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
              "r" 'magit-interactive-rebase)
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
              "q" 'magit-mode-quit-window)
            (evil-leader/set-key-for-mode 'magit-diff-mode
              ";q" 'magit-mode-quit-window)
            (evil-set-initial-state 'magit-cherry-mode 'emacs)
            (evil-define-key 'motion magit-cherry-mode-map
              "q" 'magit-mode-quit-window)
            (evil-leader/set-key-for-mode 'magit-cherry-mode
              ";q" 'magit-mode-quit-window)

            (evil-set-initial-state 'magit-log-mode 'motion)
            (evil-define-key 'motion magit-log-mode-map
              "q" 'magit-mode-quit-window)
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
              "q" 'magit-mode-quit-window)
            (evil-leader/set-key-for-mode 'magit-reflog-mode
              ";q" 'magit-mode-quit-window
              ";e" 'magit-log-show-more-entries
              ";h" 'magit-log-toggle-margin
              ";=" 'magit-diff-with-mark
              ";d" 'magit-ediff
              ";g" 'magit-refresh
              ";G" 'magit-refresh-all
              )

            (evil-set-initial-state 'magit-key-mode 'emacs)
            )
  )

(req-package git-rebase-mode
  :require evil-leader
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
  :require evil-leader
  :config (progn
            (evil-set-initial-state 'git-commit-mode 'normal)
            (evil-leader/set-key-for-mode 'git-commit-mode
              ";w" 'git-commit-commit
              ";q" 'git-commit-abort
              ";a" 'git-commit-abort
              )
            )
  )

(req-package evil-god-state
  :require evil-leader
  :config (progn
            (evil-leader/set-key
              "eg" 'evil-execute-in-god-state)
            (evil-define-key 'god global-map [escape] 'evil-god-state-bail)
            )
  )

;; (req-package evil-visual-mark-mode
;;   :config (evil-visual-mark-mode))

(req-package helm
  :require evil-leader
  :diminish helm-mode
  :bind (
         ("M-x" . helm-M-x)
         )
  :config (progn
            (global-set-key (kbd "C-c h") 'helm-command-prefix)
            (global-unset-key (kbd "C-x c"))
            (evil-leader/set-key
              "b" 'helm-buffers-list
              )
            (when (executable-find "curl")
              (setq helm-google-suggest-use-curl-p t)
              )
            (setq helm-split-window-in-side-p t
                  helm-move-to-line-cycle-in-source t
                  helm-ff-search-library-in-sexp t
                  helm-scroll-amount 8
                  helm-ff-file-name-history-use-recentf t
                  )
            (evil-leader/set-key
              "f" 'helm-find-files
              "Hf" 'helm-find-files
              "ev" (lambda () (interactive)
                     (helm-find-files-1 (expand-file-name
                                         komitee/emacs-config-directory)))
              "Hk" 'helm-show-kill-ring
              )
            (helm-mode 1)
            (helm-autoresize-mode t)
            )
  )

(req-package helm-ag)

(req-package helm-projectile
  :require evil-leader ag
  :config (progn
            (evil-leader/set-key
              "p" 'helm-projectile
              "P" 'helm-projectile-switch-project
              "B" 'helm-projectile-switch-to-buffer
              "r" 'helm-projectile-recentf
              "/" 'helm-projectile-ag
              "Ha" 'helm-projectile-ag
              )
            (evil-ex-define-cmd "ag" 'helm-projectile-ag)
            )
  )

(req-package helm-c-yassnippet
  :require evil-leader yasnippet
  :config (progn
            (evil-leader/set-key
              "Hy" 'helm-yas-complete
              )
            )
  )
