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
                      evil-leader
                      textmate
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
(require 'evil-leader)

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

(defmacro allow-line-as-region-for-function (orig-function)
`(defun ,(intern (concat (symbol-name orig-function) "-or-line"))
   ()
   ,(format "Like `%s', but acts on the current line if mark is not active."
            orig-function)
   (interactive)
   (if mark-active
       (call-interactively (function ,orig-function))
     (save-excursion
       ;; define a region (temporarily) -- so any C-u prefixes etc. are preserved.
       (beginning-of-line)
       (set-mark (point))
       (end-of-line)
       (call-interactively (function ,orig-function))))))

(allow-line-as-region-for-function comment-or-uncomment-region)

(require 'textmate)
(textmate-mode)

(evil-leader/set-leader "SPC")
(evil-leader/set-key "SPC" 'comment-or-uncomment-region-or-line)
(evil-leader/set-key "d" 'speedbar)
(evil-leader/set-key "b" 'ido-display-buffer)
(evil-leader/set-key "p" 'textmate-goto-file)

(server-start)
