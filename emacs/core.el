(req-package better-defaults)

;; minibuffer history
(req-package savehist
  :config (savehist-mode +1)
  )

;; recent files
(req-package recentf
  :config (recentf-mode +1)
  )

;; interatively do things ...
(req-package ido
  :config (progn
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

;; Side-by-side diff is superior.
(setq ediff-split-window-function 'split-window-horizontally)

;; Relocate customiztions
(setq custom-file (concat komitee/emacs-config-directory "custom.el"))

; Emacs now has a good editor.
(setq evil-emacs-state-cursor '("red" box)
      evil-normal-state-cursor '("white" box)
      evil-insert-state-cursor '("white" bar)
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
  :config (global-evil-jumper-mode))

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


;; Update modes, everything that defaults to emacs state should
;; instead default to motion state. Anything that requires editing,
;; we'll remove from motion state so it's in notmal mode by default,
;; unless there's a VERY good reason to use emacs state.
(setq evil-motion-state-modes (append evil-emacs-state-modes
                                      evil-motion-state-modes))
(setq evil-emacs-state-modes nil)
(setq evil-motion-state-modes (remove 'Custom-mode evil-motion-state-modes))

;; We want _ to be considered a word character, like it is in vim.
(modify-syntax-entry ?_ "w")


;; Projectile for better fuzzy matching and more
(req-package projectile
  :require evil ag
  :config (progn
            (req-package ag
              :bind ("C-c /" . ag-regexp-project-at-point)
              :config (setq ag-highlight-search t))
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
  :config (yas-global-mode 1))

(req-package ace-window
  :require evil
  :config (define-key evil-normal-state-map (kbd "C-w ?") 'ace-window)
  )
