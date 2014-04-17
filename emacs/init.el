(defvar-local global-emacs-config-directory "~/.dotfiles/emacs/")
(defun komitee/load (name)
  (load-file (concat global-emacs-config-directory "/lib/" name ".el")))

(komitee/load "packages")
(komitee/load "utils")
(komitee/load "core")
(komitee/load "evil")
(komitee/load "decorations")
(komitee/load "autocomplete")
(komitee/load "coding")
(komitee/load "misc")
(komitee/load "maps")

;; Apply local customizations
(defvar-local local-emacs-config (concat user-emacs-directory "local.el"))
(when (file-exists-p local-emacs-config)
          (load-file local-emacs-config))

;; If we're in a window system of any kind start the server
(require 'server)
(when window-system
  (setq server-use-tcp t
        server-host system-name)
  (server-start)
  (server-mode))
