;; Enabling to gain access to the emacs-starter-kit because I don't
;; know what I'm doing. https://github.com/technomancy/emacs-starter-kit/
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; These are the packages I want installed
(defvar my-packages '(starter-kit
                      starter-kit-lisp
                      puppet-mode
                      starter-kit-bindings
                      starter-kit-eshell
                      evil
                      textmate
                      color-theme
                      fill-column-indicator
                      markdown-mode
                      python-mode
                      )
  "A list of packages to ensure are installed at launch.")

;; This ensures they're installed
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; add our additional libraries to our  load path
(add-to-list 'load-path "~/.dotfiles/emacs/lib")

;; Enable line numbers
(global-linum-mode t)

(require 'textmate)
(textmate-mode)

;; Color-column
(require 'fill-column-indicator)
(setq fci-rule-width 1)
(setq fci-rule-color "darkred")

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
(setq evil-default-state 'normal)
(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode 1)

;; emulates surround.vim
(require 'surround)
(global-surround-mode 1)

;; escape to ... escape.
(define-key minibuffer-local-map [escape] 'keyboard-escape-quit)
(define-key minibuffer-local-ns-map [escape] 'keyboard-escape-quit)
(define-key minibuffer-local-completion-map [escape] 'keyboard-escape-quit)
(define-key minibuffer-local-must-match-map [escape] 'keyboard-escape-quit)
(define-key minibuffer-local-isearch-map [escape] 'keyboard-escape-quit)

;; Emulate some of my maps from vim
(define-key evil-normal-state-map "\C-j" 'evil-window-down)
(define-key evil-normal-state-map "\C-k" 'evil-window-up)
(define-key evil-normal-state-map "\C-h" 'evil-window-left)
(define-key evil-normal-state-map "\C-l" 'evil-window-right)
(define-key evil-normal-state-map "|" (kbd ":vsplit C-m C-l"))
(define-key evil-normal-state-map "_" (kbd ":split C-m C-j"))
(define-key evil-normal-state-map " b" 'ido-display-buffer)
(define-key evil-normal-state-map " p" 'textmate-goto-file)
(define-key evil-normal-state-map " ev" (kbd ":e ~/.dotfiles/emacs/init.el"))
(define-key evil-normal-state-map " cc" 'comment-or-uncomment-region-or-line)
(define-key evil-visual-state-map " cc" 'comment-or-uncomment-region-or-line)
(define-key evil-visual-state-map " s" 'sort-lines)
(define-key evil-normal-state-map "j" (kbd "gj"))
(define-key evil-normal-state-map "k" (kbd "gk"))

;; Since there's no 'noremap' functionality available, I have to first
;; define a sequence of characters to perform the shift, and THEN
;; remap > and < to call that other sequence and then gv, this works
;; but it makes me sad.

(define-key evil-visual-state-map "g>" 'evil-shift-right)
(define-key evil-visual-state-map "g<" 'evil-shift-left)
(define-key evil-visual-state-map ">" (kbd "g>gv"))
(define-key evil-visual-state-map "<" (kbd "g<gv"))

;; I switch ' and ` in vim, so I do so here as well
(define-key evil-motion-state-map "'" 'evil-goto-mark)
(define-key evil-motion-state-map "`" 'evil-goto-mark-line)

;; Arrow keys to resize splits
;; (define-key evil-normal-state-map [up] (kbd "C-w +"))
;; (define-key evil-normal-state-map [down] (kbd "C-w -"))
;; (define-key evil-normal-state-map [left] (kbd "C-w <"))
;; (define-key evil-normal-state-map [right] (kbd "C-w >"))

;; Here's how to define a new ex command
(evil-ex-define-cmd "Q" 'evil-quit)
(evil-ex-define-cmd "QA" 'evil-quit-all)
(evil-ex-define-cmd "Qa" 'evil-quit-all)
(evil-ex-define-cmd "WQ" 'evil-save-and-close)
(evil-ex-define-cmd "Wq" 'evil-save-and-close)
(evil-ex-define-cmd "esh[ell]" 'eshell)
(evil-ex-define-cmd "sort" 'sort-lines)

;; We prefer normal mode in several places
(defvar my-normal-modes'(
                         package-menu-mode
                         )
  "List of modes to put in normal mode by default, despite evil defaults")

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

;; emacs 24.1 doesn't have plist-to-alist, but color themes still use
;; it, ... so we define it.
(defun plist-to-alist (the-plist)
  (defun get-tuple-from-plist (the-plist)
    (when the-plist
      (cons (car the-plist) (cadr the-plist))))

  (let ((alist '()))
    (while the-plist
      (add-to-list 'alist (get-tuple-from-plist the-plist))
      (setq the-plist (cddr the-plist)))
      alist))

(require 'color-theme)
(color-theme-tty-dark)

(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)
(remove-hook 'prog-mode-hook 'esk-turn-on-idle-highlight-mode)
(add-hook 'prog-mode-hook 'esk-turn-on-whitespace)
(add-hook 'prog-mode-hook 'fci-mode)
(add-hook 'prog-mode-hook
          (lambda ()
            (setq fci-rule-column 80)))

(setq python-python-command "ipython")

(setq auto-mode-alist
      (append
       ;; File name (within directory) starts with a dot.
       '(("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.pp\\'" . puppet-mode))
                    auto-mode-alist))

;; all numbers are Unicode codepoint in decimal.
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

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq ido-enable-flex-matching t)

(server-start)
