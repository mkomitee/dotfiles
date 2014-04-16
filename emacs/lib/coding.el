;; General
(show-paren-mode t)
(which-function-mode t)
(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)
(add-hook 'before-save-hook 'whitespace-cleanup)
(electric-indent-mode t)

(require-package 'fill-column-indicator)
(require 'fill-column-indicator)
(setq fci-rule-width 1)
(setq fci-rule-color "darkred")
(setq fci-rule-character ?\u254e)
(add-hook 'prog-mode-hook 'fci-mode)
(add-hook 'prog-mode-hook
          (lambda ()
            (setq fci-rule-column 80)))

(require-package 'yasnippet)
(require 'yasnippet)
(yas-global-mode t)

(evil-define-key 'insert yas-minor-mode-map (kbd "TAB")
  'yas-next-field-or-maybe-expand)

(require-package 'flycheck)
(after 'flycheck
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers)))
(global-flycheck-mode t)

;; Lisp
(require-package 'rainbow-delimiters)
(require-package 'paredit)

(defun my-lisp-hook ()
  (progn
    (turn-on-eldoc-mode)
    (require 'evil-paredit)
    (paredit-mode t)
    (evil-paredit-mode t)
    (rainbow-delimiters-mode t)))

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
