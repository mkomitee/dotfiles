;; evil is too fundamental and its functions are used too much
;; throughout the rest of my config to use req-package ... and evil
;; requires undo-tree.
(defvar komitee/packages '(evil
                           undo-tree
                           req-package))

(require 'package)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (package komitee/packages)
  (unless (package-installed-p package)
    (package-install package)))

(require 'req-package)
