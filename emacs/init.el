(require 'package)
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(set 'global-emacs-config-directory "~/.dotfiles/emacs/")
(defun komitee/load (name)
  (load-file (concat global-emacs-config-directory "/lib/" name ".el")))

(komitee/load "utils")
(komitee/load "core")
(komitee/load "evil")
(komitee/load "decorations")
(komitee/load "autocomplete")
(komitee/load "coding")
(komitee/load "prose")
(komitee/load "misc")
(komitee/load "maps")

;; Apply local customizations
(set 'local-emacs-config (concat user-emacs-directory "local.el"))
(when (file-exists-p local-emacs-config)
          (load-file local-emacs-config))

;; If we're in a window system of any kind start the server
(when window-system
  (setq server-use-tcp t
        server-host system-name)
  (server-start)
  (server-mode))
