(setq auto-save-list-file-prefix "~/.emacs.d/.auto-save-list/.saves-"
      inhibit-startup-echo-area-message t
      inhibit-startup-screen t
      frame-background-mode (quote dark)
      indicate-buffer-boundaries (quote left)
      scroll-conservatively 9999
      require-final-newline t
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
(add-hook 'text-mode-hook
          (lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'prog-mode-hook
          (lambda () (modify-syntax-entry ?_ "w")))


(req-package ag
  :require evil
  :config (progn
            (setq ag-reuse-buffers t
                  ag-reuse-window t)
            ;; (evil-leader/set-key
            ;;   "/t" 'ag-project-files ;; Prompts for a FILE-TYPE
            ;;   )
            ;; Not sure why this isn't the default, but I'd prefer to
            ;; auto-switch to the ag buffer
            (add-hook 'ag-mode-hook (lambda () (pop-to-buffer "*ag search*")))
            )
  )

;; Projectile for better fuzzy matching and more
(req-package projectile
  :require (evil-leader ag)
  :config (progn
            (setq projectile-cache-file "~/.emacs.d/.projectile.cache"
                  projectile-known-projects-file "~/.emacs.d/.projectile-bookmarks.eld"
                  projectile-require-project-root nil
                  projectile-enable-caching t
                  projectile-file-exists-local-cache-expire 30
                  projectile-enable-idle-timer t
                  projectile-ignored-projects (append
                                               (komitee/split-file "~/.emacs.d/.projectile-ignored-projects")
                                               (mapcar (lambda (p) (concat "~/.emacs.d/elpa/" p))
                                                       (directory-files "~/.emacs.d/elpa"))))
            ;; (evil-leader/set-key
            ;;   "fp" 'projectile-find-file
            ;;   "fP" 'projectile-switch-project
            ;;   "fB" 'projectile-switch-to-buffer
            ;;   "fr" 'projectile-recentf
            ;;   "//" 'projectile-ag
            ;;   )
            (projectile-global-mode t)
            )
  )

(req-package autorevert
  :config (global-auto-revert-mode 1))

;; Allows completion for commands.
;; (req-package smex
;;   :bind (("M-x" . smex)
;;          ("C-x C-m" . smex)
;;          ("C-c C-m" . smex))
;;   :config (progn
;;             (setq smex-save-file "~/.emacs.d/.smex-items")
;;             (smex-initialize)
;;             )
;;   )

;; Snippets are useful
(req-package yasnippet
  :diminish yas-minor-mode
  :require evil
  :config (progn
            (setq yas-prompt-functions '(yas-ido-prompt))
            (yas-global-mode 1)

            (defun komitee/snippets ()
              "Lets you select (and expand) a yasnippet key"
              (interactive)
              (let ((original-point (point)))
                (while (and
                        (not (= (point) (point-min) ))
                        (not
                         (string-match "[[:space:]\n]" (char-to-string (char-before)))))
                  (backward-word 1))
                (let* ((init-word (point))
                       (word (buffer-substring init-word original-point))
                       (list (yas-active-keys)))
                  (goto-char original-point)
                  (let ((key (remove-if-not
                              (lambda (s) (string-match (concat "^" word) s)) list)))
                    (if (= (length key) 1)
                        (setq key (pop key))
                      (setq key (ido-completing-read "key: " list nil nil word)))
                    (delete-char (- init-word original-point))
                    (insert key)
                    (yas-expand)))))

            (evil-define-key 'insert global-map
              (kbd "<S-tab>") 'komitee/snippets
              )
            )
  )

(req-package ace-window
  :require evil
  :config (progn
            (setq aw-leading-char-style 'path)
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
  :config (progn
            (setq fci-rule-character 9550
                  fci-rule-color "darkred"
                  fci-rule-column 80
                  )
            (add-hook 'prog-mode-hook 'turn-on-fci-mode)
            (add-hook 'text-mode-hook 'turn-on-fci-mode)
            )
  )

(req-package crontab-mode
  :config (progn
            (add-to-list 'auto-mode-alist '("\\.cron\\(tab\\)?\\'" . crontab-mode))
            (add-to-list 'auto-mode-alist '("cron\\(tab\\)?\\."    . crontab-mode))
            )
  )

(req-package js2-mode
  :config (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  )

(req-package ssh-config-mode
  :config (progn
            (add-to-list 'auto-mode-alist '(".ssh/config\\'"       . ssh-config-mode))
            (add-to-list 'auto-mode-alist '("sshd?_config\\'"      . ssh-config-mode))
            (add-to-list 'auto-mode-alist '("known_hosts\\'"       . ssh-known-hosts-mode))
            (add-to-list 'auto-mode-alist '("authorized_keys2?\\'" . ssh-authorized-keys-mode))
            )
  )

(req-package apache-mode)
(req-package cmake-mode)
(req-package csv-mode)
(req-package yaml-mode)
(req-package haml-mode)
(req-package jinja2-mode)
(req-package less-css-mode)
(req-package json-mode)
(req-package llvm-mode)
(req-package mustache-mode)
(req-package nginx-mode)
(req-package nix-mode)
(req-package protobuf-mode)
(req-package rpm-spec-mode)
(req-package rust-mode)
(req-package sass-mode)
(req-package scss-mode)
(req-package toml-mode)
(req-package vimrc-mode)

(req-package discover
  :config (global-discover-mode)
  )

(req-package buffer-move
  :require evil
  :config (progn
            (define-key evil-window-map (kbd "S-h") 'buf-move-left)
            (define-key evil-window-map (kbd "S-l") 'buf-move-right)
            (define-key evil-window-map (kbd "S-j") 'buf-move-down)
            (define-key evil-window-map (kbd "S-k") 'buf-move-up)
            (define-key global-map (kbd "C-S-h") 'buf-move-left)
            (define-key global-map (kbd "C-S-l") 'buf-move-right)
            (define-key global-map (kbd "C-S-j") 'buf-move-down)
            (define-key global-map (kbd "C-S-k") 'buf-move-up)
            )
  )

(req-package evil-org
  :require (evil evil-leader org)
  )

(req-package eyebrowse
  :require evil
  :diminish eyebrowse-mode
  :config (progn
            (eyebrowse-mode t)
            (setq eyebrowse-new-workspace t
                  eyebrowse-wrap-around t)
            (evil-define-key 'motion global-map
              "[t" 'eyebrowse-prev-window-config
              "]t" 'eyebrowse-next-window-config
              "gT" 'eyebrowse-prev-window-config
              "gt" 'eyebrowse-next-window-config
              )
            (evil-ex-define-cmd "tabn[ew]" 'eyebrowse-switch-to-window-config)
            (evil-ex-define-cmd "tabc[lose]" 'eyebrowse-close-window-config)
            )
  )
