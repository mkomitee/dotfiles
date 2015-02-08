(setq auto-save-list-file-prefix "~/.emacs.d/.auto-save-list/.saves-"
      inhibit-startup-echo-area-message t
      inhibit-startup-screen t
      initial-buffer-choice (quote remember-notes)
      frame-background-mode (quote dark)
      indicate-buffer-boundaries (quote left)
      scroll-conservatively 9999
      scroll-up-aggressively 0.0
      scroll-down-aggressively 0.0
      scroll-margin 3
      scroll-preserve-screen-position t
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
                uniquify-buffer-name-style 'post-forward
                uniquify-separator "::"
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

(defalias 'yes-or-no-p 'y-or-n-p)
(xterm-mouse-mode t)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(blink-cursor-mode -1)

(req-package undo-tree
  :diminish undo-tree-mode
  :config (progn
            (setq undo-tree-auto-save-history t
                  undo-tree-history-directory-alist (quote (("." . "~/.emacs.d/.undo")))
                  )
            (global-undo-tree-mode)
            )
  )

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

;; We want _ to be considered a word character, like it is in vim.
(modify-syntax-entry ?_ "w")

(req-package ag
  :require evil
  ;; This shouldn't be necessary, but adding ag-mode to
  ;; evil-motion-state-modes doesn't seem to have the desired effect.
  :config (add-hook 'ag-mode-hook 'evil-motion-state)
  )

;; Projectile for better fuzzy matching and more
(req-package projectile
  :require evil-leader ag
  :config (progn
            (setq projectile-cache-file "~/.emacs.d/.projectile.cache"
                  projectile-known-projects-file "~/.emacs.d/.projectile-bookmarks.eld"
                  projectile-require-project-root nil
                  projectile-enable-caching t
                  projectile-ignored-projects (append
                                               (komitee/split-file "~/.emacs.d/.projectile-ignored-projects")
                                               (directory-files "~/.emacs.d/elpa" t)))
            (evil-leader/set-key
              "p" 'projectile-find-file
              "B" 'projectile-switch-to-buffer
              "r" 'projectile-recentf
              "/" 'projectile-ag
              )
            (projectile-global-mode t)
            )
  )

(req-package persp-projectile
  :require perspective evil-leader
  :config (evil-leader/set-key
              "P" 'projectile-persp-switch-project
              )

  )

(req-package perspective
  :config (progn
            (evil-define-key 'motion global-map
              "]t" 'persp-next
              "[t" 'persp-prev
              )
            (evil-leader/set-key
              "t" 'persp-switch
              )
            (persp-mode)
            )
  )

;; Allows completion for commands.
(req-package smex
  :bind (("M-x" . smex)
         ("C-x C-m" . smex)
         ("C-c C-m" . smex))
  :config (progn
            (setq smex-save-file "~/.emacs.d/.smex-items")
            (smex-initialize)
            )
  )

;; Snippets are useful
(req-package yasnippet
  :diminish yas-minor-mode
  :config (yas-global-mode 1)
  )

(req-package ace-window
  :require evil
  :config (progn
            (define-key evil-window-map "?" 'ace-window)
            (define-key evil-window-map "S" 'ace-swap-window)
            (define-key evil-window-map "x" 'delete-other-windows)
            (define-key evil-window-map "z" 'ace-maximize-window)
            (define-key evil-window-map (kbd "DEL") 'ace-delete-window)
            )
  )

(req-package discover-my-major
  :require evil-leader
  :config (evil-leader/set-key
            "hM" 'discover-my-major
            )
  )

;; Display a thin red vertical line at the 80th column
(req-package fill-column-indicator
  :diminish fci-mode
  :config (progn
            (setq fci-rule-character 9550
                  fci-rule-color "darkred"
                  fci-rule-column 80
                  )
            (add-hook 'prog-mode-hook 'turn-on-fci-mode)
            (add-hook 'text-mode-hook 'turn-on-fci-mode)
            )
  )
