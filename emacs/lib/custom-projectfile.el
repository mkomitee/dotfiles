(defvar projectile-cache-file (concat user-emacs-directory ".cache/projectile.cache"))
(defvar projectile-known-projects-file (concat user-emacs-directory ".cache/projectile-bookmarks.eld"))

(require-package 'projectile)
(require 'projectile)
(require-package 'helm-projectile)
(require 'helm-projectile)

(add-to-list 'projectile-globally-ignored-directories "elpa")
(add-to-list 'projectile-globally-ignored-directories ".cache")

(projectile-global-mode t)
(setq projectile-require-project-root nil)

(provide 'custom-projectfile)
