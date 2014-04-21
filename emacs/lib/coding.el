;; General

;; No tabs while coding
(defun komitee/notabs ()
  (setq indent-tabs-mode nil))
(add-hook 'prog-mode-hook 'komitee/notabs)

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

;; Cleanup whitespace on save
(defun komitee/whitespace-hook ()
  (add-hook 'before-save-hook 'whitespace-cleanup nil t))
(add-hook 'prog-mode-hook 'komitee/whitespace-hook)

;; Display a thin red vertical line at the 80th column
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
(require 'yasnippet)
(yas-global-mode t)
(evil-define-key 'insert yas-minor-mode-map (kbd "TAB")
  'yas-next-field-or-maybe-expand)

;; automatically check file syntax at start, save & when idle
(after 'flycheck
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers)))
(global-flycheck-mode t)
(defun komitee/flycheck-hook ()
  (if (eq (length flycheck-current-errors) 0)
      (if (get-buffer flycheck-error-list-buffer)
          (delete-windows-on flycheck-error-list-buffer))
    (progn
      (flycheck-list-errors)
      (minimize-window (get-buffer-window flycheck-error-list-buffer)))))
(add-hook 'flycheck-after-syntax-check-hook 'komitee/flycheck-hook)

;; Lisp
(defun komitee/rainbow-hook ()
  (rainbow-delimiters-mode t))
(add-hook 'prog-mode-hook 'komitee/rainbow-hook)

(require 'evil-paredit)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode)

;; Python
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

(add-hook 'python-mode-hook 'annotate-pdb)
(add-hook 'python-mode-hook 'jedi:setup)

(evil-define-key 'insert python-mode-map (kbd "RET") 'evil-ret-and-indent)

;; make
(defun komitee/tabs ()
  (setq indent-tabs-mode t))
(add-hook 'makefile-mode-hook 'komitee/tabs)

;; haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-to-list 'completion-ignored-extensions ".hi")
(setq haskell-program-name "ghci")
