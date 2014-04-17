;; This enables vim compatible bindings, ...

(defvar evil-default-state 'normal)
(defvar evil-want-C-u-scroll t)
(defvar evil-want-C-w-in-emacs-state t)
(defvar evil-search-module 'evil-search)
(defvar evil-magic 'very-magic)
(defvar evil-emacs-state-cursor '("red" box))
(defvar evilnc-hotkey-comment-operator "gc")

(require-package 'evil)
(require-package 'evil-leader)
(require-package 'evil-matchit)
(require-package 'evil-visualstar)
(require-package 'evil-nerd-commenter)
(require-package 'evil-indent-textobject)
(require-package 'surround)

(require 'evil)
(require 'evil-leader)
(require 'evil-nerd-commenter)
(require 'evil-indent-textobject)
(require 'evil-visualstar)
(require 'surround)

(add-to-list 'evil-emacs-state-modes 'profiler-report-mode)
(add-to-list 'evil-emacs-state-modes 'ag-mode)
(add-to-list 'evil-emacs-state-modes 'compilation)
(add-to-list 'evil-emacs-state-modes 'help)
(add-to-list 'evil-emacs-state-modes 'package-menu-mode)
(add-to-list 'evil-emacs-state-modes 'inferior-python-mode)
(add-to-list 'evil-emacs-state-modes 'inferior-haskell-mode)

(add-to-list 'evil-normal-state-modes 'flycheck-error-list-mode)

(evil-mode 1)
(global-surround-mode 1)
(global-evil-matchit-mode 1)

(evil-define-text-object my-evil-next-match (count &optional beg end type)
  "Select next match."
  (evil-ex-search-previous 1)
  (evil-ex-search-next count)
  (list evil-ex-search-match-beg evil-ex-search-match-end))

(evil-define-text-object my-evil-previous-match (count &optional beg end type)
  "Select previous match."
  (evil-ex-search-next 1)
  (evil-ex-search-previous count)
  (list evil-ex-search-match-beg evil-ex-search-match-end))

(define-key evil-motion-state-map "gn" 'my-evil-next-match)
(define-key evil-motion-state-map "gN" 'my-evil-previous-match)

(defadvice evil-ex-search-next (after advice-for-evil-ex-search-next activate)
  (evil-scroll-line-to-center (line-number-at-pos)))

(defadvice evil-ex-search-previous (after advice-for-evil-ex-search-previous activate)
  (evil-scroll-line-to-center (line-number-at-pos)))

(evil-define-command custom/evil-force-normal-state ()
  "Switch to normal state without recording current command.
Cancel highlight search"
  :repeat abort
  :suppress-operator t
  ()
  (evil-ex-nohighlight)
  (evil-normal-state))

(defun my-move-key (keymap-from keymap-to key)
  "Moves key binding from one keymap to another, deleting from the old location. "
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil))
(my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))

(add-to-list 'evil-emacs-state-modes 'direx:direx-mode)
