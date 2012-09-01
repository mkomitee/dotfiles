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
                      fill-column-indicator
                      color-theme-molokai
                      )
  "A list of packages to ensure are installed at launch.")

;; This ensures they're installed
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Enable line numbers
(global-linum-mode t)

;; Color-column
(require 'fill-column-indicator)
(setq fci-rule-width 1)
(setq fci-rule-color "darkred")
(add-hook 'after-change-major-mode-hook 'fci-mode)
(add-hook 'after-change-major-mode-hook 'whitespace-mode)

(require 'textmate)
(textmate-mode)

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
       (beginning-of-line)
       (set-mark (point))
       (end-of-line)
       (call-interactively (function ,orig-function))))))

(allow-line-as-region-for-function comment-or-uncomment-region)

;; This enables vim compatible bindings, ...
(require 'evil)
(evil-mode 1)
(require 'evil-leader)
(setq evil-default-state 'normal)

(evil-leader/set-leader "SPC")
(evil-leader/set-key "SPC" 'comment-or-uncomment-region-or-line)
(evil-leader/set-key "d" 'speedbar)
(evil-leader/set-key "b" 'ido-display-buffer)
(evil-leader/set-key "p" 'textmate-goto-file)

(define-key evil-normal-state-map "\C-j" 'evil-window-down)
(define-key evil-normal-state-map "\C-k" 'evil-window-up)
(define-key evil-normal-state-map "\C-h" 'evil-window-left)
(define-key evil-normal-state-map "\C-l" 'evil-window-right)
(define-key evil-normal-state-map "\C-c" 'delete-window)
(define-key evil-normal-state-map "|" 'split-window-horizontally)
(define-key evil-normal-state-map "_" 'split-window-vertically)

(server-start)
