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
(setq yas-prompt-functions '(yas-ido-prompt
                             yas-completing-prompt
                             yas-no-prompt))

;; automatically check file syntax at start, save & when idle
(after 'flycheck
  (setq flycheck-check-syntax-automatically '(save
                                              mode-enabled
                                              new-line
                                              idle-change))
  (setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers)))
(global-flycheck-mode t)
(setq flycheck-highlighting-mode 'lines)
(setq flycheck-display-errors-delay 0)

;; Lisp
(defun komitee/rainbow-hook ()
  (rainbow-delimiters-mode t))
(add-hook 'prog-mode-hook 'komitee/rainbow-hook)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

;; Python
(elpy-enable)
(elpy-use-ipython)
(elpy-clean-modeline)
(setq elpy-default-minor-modes (delete 'flymake-mode elpy-default-minor-modes))

;; make
(defun komitee/tabs ()
  (setq indent-tabs-mode t))
(add-hook 'makefile-mode-hook 'komitee/tabs)

;; haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-to-list 'completion-ignored-extensions ".hi")
(setq haskell-program-name "ghci")
