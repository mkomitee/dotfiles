;; Enabling to gain access to the emacs-starter-kit because I don't
;; know what I'm doing. https://github.com/technomancy/emacs-starter-kit/
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; These are the packages I want installed
(defvar my-packages '(color-theme
                      color-theme-solarized
                      evil
                      fill-column-indicator
                      markdown-mode
                      puppet-mode
                      python-mode
                      starter-kit
                      starter-kit-bindings
                      starter-kit-eshell
                      starter-kit-lisp
                      textmate
                      yasnippet
                      )
  "A list of packages to ensure are installed at launch.")

;; This ensures they're installed
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; add our additional libraries to our load path
(add-to-list 'load-path "~/.dotfiles/emacs/lib")

;; Load up our utility functions, ...
(require 'utils)

;; Enable line numbers
(global-linum-mode t)

;; Textmate-mode has a nice project find which we use
(require 'textmate)
(textmate-mode)

;; Color-column
(require 'fill-column-indicator)
(setq fci-rule-width 1)
(setq fci-rule-color "darkred")

;; New command for (un)commenting lines as well as selected regions
(allow-line-as-region-for-function comment-or-uncomment-region)

;; This enables vim compatible bindings, ...
(setq evil-default-state 'normal)
(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode 1)

;; Load up my custom keymaps
(require 'maps)

;; emulates surround.vim
(require 'surround)
(global-surround-mode 1)

;; escape to ... escape in all modes
(define-key minibuffer-local-map [escape] 'keyboard-escape-quit)
(define-key minibuffer-local-ns-map [escape] 'keyboard-escape-quit)
(define-key minibuffer-local-completion-map [escape] 'keyboard-escape-quit)
(define-key minibuffer-local-must-match-map [escape] 'keyboard-escape-quit)
(define-key minibuffer-local-isearch-map [escape] 'keyboard-escape-quit)

;; We prefer normal mode in several places
(defvar my-normal-modes'(package-menu-mode))

(dolist (p my-normal-modes)
  (delete p 'evil-emacs-state-modes)
  (add-to-list 'evil-normal-state-modes p))

;; Don't litter auto-save turds all over the file system
(setq backup-directory-alist
     `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
     `((".*" ,temporary-file-directory t)))

(eval-after-load "dired"
  '(require 'dired-x))

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-omit-mode 1)))

(require 'color-theme)
;; (color-theme-tty-dark)
(color-theme-solarized-light)

(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)
(remove-hook 'prog-mode-hook 'esk-turn-on-idle-highlight-mode)
(add-hook 'prog-mode-hook 'esk-turn-on-whitespace)
(add-hook 'prog-mode-hook 'fci-mode)
(add-hook 'prog-mode-hook
          (lambda ()
            (setq fci-rule-column 80)))

;; Use ipython instead of python where possible
(setq python-python-command "ipython")

;; Set modes for files based on their filenames
(setq auto-mode-alist
      (append
       '(("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.pp\\'" . puppet-mode))
                    auto-mode-alist))

;; Configure whitespace-mode
(setq whitespace-display-mappings
      '((space-mark 32 [183] [46])
        (newline-mark 10 [8617 10])
        (tab-mark 9 [9656 9] [92 9])))

(setq whitespace-style (quote (face
                               trailing
                               newline
                               indentation
                               empty
                               tab-mark)))

;; utf-8 all the things.
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Use fuzzy matching with ido-mode
(setq ido-enable-flex-matching t)

;; snippets ftw
(require 'yasnippet)
(yas-global-mode 1)
(setq yas-snippet-dirs '("~/.dotfiles/emacs/snippets"))
