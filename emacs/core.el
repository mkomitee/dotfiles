(req-package better-defaults)

(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t)

;; minibuffer history
(require 'savehist)
(setq savehist-file (concat user-emacs-directory ".savehist")
      savehist-additional-variables '(search ring regexp-search-ring)
      savehist-autosave-interval 60)
(savehist-mode +1)

;; recent files
(require 'recentf)
(setq recentf-save-file (concat user-emacs-directory ".recentf")
      recentf-max-saved-items 100
      recentf-max-menu-items 50)
(recentf-mode +1)

;; store most files in the cache
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory ".backups/")))
      auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory ".backups/") t))
      auto-save-list-file-prefix
      (concat user-emacs-directory ".auto-save-list/.saves-")
      backup-by-copying t)

;; Don't use lockfiles. This prevents us from dropping .#foo# file
;; turds in the filesystem, but will prevent us from detecting conflicts
(setq create-lockfiles nil)

;; better scrolling
(setq scroll-margin 3
      scroll-conservatively 9999
      scroll-preserve-screen-position t)

;; better buffer names for duplicates
(setq uniquify-separator "/"
      uniquify-ignore-buffers-re "^\\*" ; leave special buffers alone
      uniquify-after-kill-buffer-p t)


;; interatively do things ...
(req-package ido
  :config (progn
            (setq ido-enable-prefix nil
                  ido-create-new-buffer 'prompt
                  ido-use-filename-at-point 'guess
                  ido-save-directory-list-file (concat user-emacs-directory ".ido.last")
                  ido-enable-flex-matching t)
            (ido-mode t)
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
            (setq smex-save-file (concat user-emacs-directory ".smex-items"))
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
(blink-cursor-mode -1)

(require 'undo-tree)
(setq undo-tree-auto-save-history t)
(setq-default undo-tree-history-directory-alist
              `(("." . ,(concat user-emacs-directory ".undo"))))
(global-undo-tree-mode)

;; make sure $PATH is set correctly
(when (fboundp 'exec-path-from-shell-initialize)
  (ignore-errors ;; windows
    (exec-path-from-shell-initialize)))

;; Use popwin to keep control of my windows
(require 'popwin)
(popwin-mode 1)
(push '("*Flycheck errors*" :position bottom :height 5) popwin:special-display-config)

;; Side-by-side diff is superior.
(setq ediff-split-window-function 'split-window-horizontally)

;; Sentences endings don't require two spaces.
(setq sentence-end-double-space nil)

;; Relocate customiztions
(setq custom-file (concat komitee/emacs-config-directory "custom.el"))

; Emacs now has a good editor.
(setq evil-want-C-u-scroll t
      evil-search-module 'evil-search
      evil-magic 'very-magic
      evil-emacs-state-cursor '("red" box)
      evil-normal-state-cursor '("white" box)
      evil-insert-state-cursor '("white" bar)
      evilnc-hotkey-comment-operator "gc")

(evil-mode 1)
(global-evil-matchit-mode 1)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(global-evil-tabs-mode)
(require 'evil-jumper)
(require 'evil-args)
(require 'evil-commentary)
(evil-commentary-default-setup)
(evilem-default-keybindings "SPC")
(require 'evil-surround)
(global-evil-surround-mode 1)
(require 'evil-snipe)
(global-evil-snipe-mode 1)
(evil-snipe-replace-evil)

;; Update modes
(setq evil-emacs-state-modes (remove 'Custom-mode evil-emacs-state-modes))
(setq evil-emacs-state-modes (remove 'help-mode evil-emacs-state-modes))

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
            (setq projectile-cache-file (concat user-emacs-directory
                                                ".projectile.cache")
                  projectile-known-projects-file (concat user-emacs-directory
                                                         ".projectile-bookmarks.eld")
                  projectile-require-project-root nil)
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
