;; General

;; Figure out how indentation should work for these languages by
;; inspecting the file. For new files, default to spaces.
(require-package 'smart-tabs-mode)
(smart-tabs-insinuate 'c 'c++ 'cperl 'java 'python 'ruby)

;; Highlight incorrect use of whitespace
(require 'whitespace)
(setq whitespace-style '(face
                         faces
                         space-before-tab
                         tab-mark
                         tabs
                         trailing))
(add-hook 'prog-mode-hook 'whitespace-mode)

;; Show matching parenthesis
(show-paren-mode t)

(add-hook 'before-save-hook 'whitespace-cleanup)
(electric-indent-mode t)

;; Display a thin red vertical line at the 80th column
(require-package 'fill-column-indicator)
(require 'fill-column-indicator)
(setq fci-rule-width 1)
(setq fci-rule-color "darkred")
(setq fci-rule-character ?\u254e)
(defun komitee/fci-hook ()
  (progn
    (turn-on-fci-mode)
    (setq fci-rule-column 80)))
(add-hook 'prog-mode-hook 'komitee/fci-hook)

;; Snippets are useful!
(require-package 'yasnippet)
(require 'yasnippet)
(yas-global-mode t)
(evil-define-key 'insert yas-minor-mode-map (kbd "TAB")
  'yas-next-field-or-maybe-expand)

;; automatically check file syntax at start, save & when idle
(require-package 'flycheck)
(after 'flycheck
  (setq flycheck-check-syntax-automatically '(save mode-enabled idle-change))
  (setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers)))
(global-flycheck-mode t)
(defun komitee/flycheck-hook ()
  (if (eq (length flycheck-current-errors) 0)
      (delete-windows-on flycheck-error-list-buffer)
    (flycheck-list-errors)))
(add-hook 'flycheck-after-syntax-check-hook 'komitee/flycheck-hook)

;; Lisp
(require-package 'rainbow-delimiters)
(defun komitee/rainbow-hook ()
  (rainbow-delimiters-mode t))
(add-hook 'prog-mode-hook 'komitee/rainbow-hook)

(require-package 'paredit)

(defun my-lisp-hook ()
  (progn
    (turn-on-eldoc-mode)
    (require 'evil-paredit)
    (paredit-mode t)
    (evil-paredit-mode t)))

(add-hook 'emacs-lisp-mode-hook 'my-lisp-hook)
(add-hook 'lisp-interaction-mode-hook 'my-lisp-hook)
(add-hook 'ielm-mode-hook 'my-lisp-hook)

;; Python
(require-package 'python)
(require-package 'jedi)
(require-package 'jedi-direx)

(setq
 jedi:complete-on-dot t
 jedi:tooltip-method nil
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \[[0-9]+\]: "
 python-shell-prompt-output-regexp "Out\[[0-9]+\]: "
 python-shell-completion-setup-code ""
 python-shell-completion-string-code "';'.join(get_ipython().complete('''%s''')[1])\n")

(add-hook 'jedi-mode-hook 'jedi-direx:setup)

; Highlight the call to ipdb
; src http://pedrokroger.com/2010/07/configuring-emacs-as-a-python-ide-2/
(defun annotate-pdb ()
  (interactive)
  (highlight-phrase "import ipdb")
  (highlight-phrase "ipdb.set_trace()")
  (highlight-phrase "import pdb")
  (highlight-phrase "pdb.set_trace()"))

(defun my-python-hook ()
  (progn
    (annotate-pdb)
    (fci-mode)
    (setq fci-rule-column 80)
    (jedi:setup)))

(add-hook 'python-mode-hook 'my-python-hook)

(evil-define-key 'insert python-mode-map (kbd "RET") 'evil-ret-and-indent)

;; make
(add-hook 'makefile-mode-hook 'indent-tabs-mode)

;; haskell
(require-package 'haskell-mode)

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-to-list 'completion-ignored-extensions ".hi")
(setq haskell-program-name "ghci")

;; puppet
(require-package 'puppet-mode)
