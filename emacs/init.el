;; Enabling to gain access to the emacs-starter-kit because I don't
;; know what I'm doing. https://github.com/technomancy/emacs-starter-kit/
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; These are the packages I wabnt installed
(defvar my-packages '(starter-kit
                      starter-kit-lisp
                      starter-kit-bindings
                      evil
                      color-theme
                      color-theme-molokai)
  "A list of packages to ensure are installed at launch.")

;; This ensures they're installed
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; This enables vim compatible bindings, ...
(require 'evil)
(evil-mode 1)

;; Enable line numbers
(global-linum-mode t)

;; No idea what a good emacs theme is so using molokai which is decent
(require 'color-theme)
(color-theme-molokai)

;; Enable flymake/pylint, note this only works when epylint is in my
;; path, which isn't the case at home when started with spotlight. It
;; has to be started from the commandline.
(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "epylint" (list local-file))))
  
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))

(server-start)
