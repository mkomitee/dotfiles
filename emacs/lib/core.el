(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(unless (display-graphic-p) (menu-bar-mode -1))

(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t)

;; move cursor to the last position upon open
(require 'saveplace)
(setq save-place-file (concat user-emacs-directory ".cache/places"))
(setq-default save-place t)

;; minibuffer history
(require 'savehist)
(setq savehist-file (concat user-emacs-directory ".cache/savehist")
      savehist-additional-variables '(search ring regexp-search-ring)
      savehist-autosave-interval 60)
(savehist-mode +1)

;; recent files
(require 'recentf)
(setq recentf-save-file (concat user-emacs-directory ".cache/recentf")
      recentf-max-saved-items 100
      recentf-max-menu-items 50)
(recentf-mode +1)

;; store most files in the cache
(setq backup-directory-alist
      `((".*" . ,(concat user-emacs-directory ".cache/backups")))
      auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory ".cache/backups") t))
      auto-save-list-file-prefix
      (concat user-emacs-directory ".cache/auto-save-list/.saves-"))

;; better scrolling
(setq scroll-margin 3
      scroll-conservatively 9999
      scroll-preserve-screen-position t)

;; better buffer names for duplicates
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      uniquify-ignore-buffers-re "^\\*" ; leave special buffers alone
      uniquify-after-kill-buffer-p t)


;; interatively do things ...
(defvar ido-enable-prefix nil)
(defvar ido-enable-flex-matching t)
(defvar ido-create-new-buffer 'prompt)
(defvar ido-use-filename-at-point 'guess)
(defvar ido-save-directory-list-file (concat user-emacs-directory ".cache/ido.last"))

(require 'ido)
(ido-mode t)
(ido-everywhere t)
(ido-ubiquitous-mode t)
(flx-ido-mode t)
(ido-vertical-mode)
(defvar smex-save-file (concat user-emacs-directory ".cache/smex-items"))
(require 'smex)
(smex-initialize)

;; Projectile for better fuzzy matching and more
(defvar projectile-cache-file (concat user-emacs-directory ".cache/projectile.cache"))
(defvar projectile-known-projects-file (concat user-emacs-directory ".cache/projectile-bookmarks.eld"))
(require 'projectile)
(add-to-list 'projectile-globally-ignored-directories "elpa")
(add-to-list 'projectile-globally-ignored-directories ".cache")
(projectile-global-mode t)
(setq projectile-require-project-root nil)

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
              `(("." . ,(concat user-emacs-directory ".cache/undo"))))
(global-undo-tree-mode)

(require 'multiple-cursors)
(after 'evil
  (add-hook 'multiple-cursors-mode-enabled-hook 'evil-emacs-state)
  (add-hook 'multiple-cursors-mode-disabled-hook 'evil-normal-state))

;; If we're in X-windows, ...
(if (eq window-system 'X)
    ;; Enable x-clipboard integration
    (setq x-select-enable-clipboard t
          x-select-enable-primary t)
  )

;; make sure $PATH is set correctly
(when (fboundp 'exec-path-from-shell-initialize)
  (ignore-errors ;; windows
    (exec-path-from-shell-initialize)))
