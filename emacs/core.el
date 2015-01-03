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
  :config (setq uniquify-ignore-buffers-re "^\\*")
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
  :config (evil-commentary-default-setup)
  )

(req-package evil-easymotion
  :config (evilem-default-keybindings "SPC")
  )

(req-package evil-indent-textobject)

(req-package evil-jumper
  :config (global-evil-jumper-mode)
  )

(req-package evil-matchit
  :config (global-evil-matchit-mode 1)
  )

(req-package evil-snipe
  :config (progn
            (global-evil-snipe-mode 1)
            (evil-snipe-replace-evil)
            )
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
            (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
            (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)
            )
  )


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
              :config (setq ag-highlight-search t)
              )
            (projectile-global-mode t)
            (evil-ex-define-cmd "ag" 'projectile-ag)
            (evil-leader/set-key
              "p" 'projectile-find-file
              "r" 'projectile-recentf
              "a" 'projectile-ag
              "/" 'projectile-ag
              )
            )
  )

;; Snippets are useful
(req-package yasnippet
  :diminish yas-minor-mode
  :config (yas-global-mode 1)
  )

(req-package ace-window
  :require evil
  :config (define-key evil-normal-state-map (kbd "C-w ?") 'ace-window)
  )
