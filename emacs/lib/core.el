(require 'better-defaults)

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
(defvar ido-enable-prefix nil)
(defvar ido-create-new-buffer 'prompt)
(defvar ido-use-filename-at-point 'guess)
(defvar ido-save-directory-list-file (concat user-emacs-directory ".ido.last"))
(defvar ido-enable-flex-matching t)

(require 'ido)
(ido-mode t)
(ido-everywhere t)
(ido-ubiquitous-mode t)
(flx-ido-mode t)
(ido-vertical-mode)
(defvar smex-save-file (concat user-emacs-directory ".smex-items"))
(require 'smex)
(smex-initialize)

;; Projectile for better fuzzy matching and more
(defvar projectile-cache-file (concat user-emacs-directory ".projectile.cache"))
(defvar projectile-known-projects-file (concat user-emacs-directory ".projectile-bookmarks.eld"))
(require 'projectile)
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
(setq custom-file (concat komitee/emacs-config-directory "/lib/custom.el"))

; Emacs now has a good editor.
(setq evil-want-C-u-scroll t
      evil-search-module 'evil-search
      evil-magic 'very-magic
      evil-emacs-state-cursor '("red" box)
      evil-normal-state-cursor '("white" box)
      evil-insert-state-cursor '("white" bar)
      evilnc-hotkey-comment-operator "gc")

(evil-mode 1)
(global-surround-mode 1)
(global-evil-matchit-mode 1)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(require 'evil-nerd-commenter)

; Configure ace-jump mode
(setq ace-jump-word-mode-use-query-char nil
      ace-jump-mode-scope 'window
      ace-jump-mode-case-fold nil)

;; We want _ to be considered a word character, like it is in vim.
(modify-syntax-entry ?_ "w")

;; Snippets are useful
(require 'yasnippet)
(yas-global-mode 1)
