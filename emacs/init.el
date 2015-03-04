(defvar komitee/emacs-config-directory "~/.dotfiles/emacs/")
(defun komitee/load (name)
  (load-file (concat komitee/emacs-config-directory name ".el"))
  )

(komitee/load "custom")
(komitee/load "packages")
(komitee/load "decorations")
(komitee/load "utils")
(komitee/load "core")
(komitee/load "evil")
(komitee/load "vcs")
(komitee/load "ido")
(komitee/load "helm")
(komitee/load "coding")
(komitee/load "prose")
(komitee/load "misc")
(komitee/load "maps")

;; Apply local customizations
(defvar komitee/local-emacs-config (concat user-emacs-directory "local.el"))
(when (file-exists-p komitee/local-emacs-config)
          (load-file komitee/local-emacs-config))

;; If we're in a window system of any kind start the server
(req-package server
  :config (progn
            (setq server-host system-name
                  server-use-tcp t
                  )
            (when window-system
              (server-start)
              (server-mode))
            )
  )

(req-package-finish)
