;; evil is too fundamental and its functions are used too much
;; throughout the rest of my config to use req-package ... and evil
;; requires undo-tree.
(setq package-archives
      (quote
       (("melpa" . "http://melpa.milkbox.net/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("elpy" . "http://jorgenschaefer.github.io/packages/")))
      )
(require 'package)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (package '(evil
                   undo-tree
                   req-package
                   ;; base16-theme doesn't actually provide
                   ;; base16-theme, so it doesn't work w/ req-package, Yay.
                   base16-theme))
  (unless (package-installed-p package)
    (package-install package)))

(require 'req-package)
