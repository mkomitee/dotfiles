(defvar komitee/emacs-config-directory "~/.dotfiles/emacs/")
(defun komitee/load (name)
  (load-file (concat komitee/emacs-config-directory name ".el")))

(komitee/load "custom")
(komitee/load "packages")
(komitee/load "utils")
(komitee/load "core")
(komitee/load "decorations")
(komitee/load "coding")
(komitee/load "prose")
(komitee/load "misc")
(komitee/load "maps")

;; Apply local customizations
(defvar komitee/local-emacs-config (concat user-emacs-directory "local.el"))
(when (file-exists-p komitee/local-emacs-config)
          (load-file komitee/local-emacs-config))

(req-package-finish)

;; If we're in a window system of any kind start the server
(require 'server)
(when window-system
  (setq server-use-tcp t
        server-host system-name)
  (server-start)
  (server-mode))
