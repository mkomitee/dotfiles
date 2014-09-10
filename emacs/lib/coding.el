;; General

(add-hook 'prog-mode-hook 'flycheck-mode)

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
;; (require 'fill-column-indicator)
;; (setq fci-rule-width 1)
;; (setq fci-rule-color "darkred")
;; (setq fci-rule-character ?\u254e)
;; (defun komitee/fci-hook ()
;;   (progn
;;     (turn-on-fci-mode)
;;     (setq fci-rule-column 80)))
;; (add-hook 'prog-mode-hook 'komitee/fci-hook)

(defun komitee/comment-auto-fill ()
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode 1))
(add-hook 'prog-mode-hook 'komitee/comment-auto-fill)

;; Lisp
(defun komitee/rainbow-hook ()
  (rainbow-delimiters-mode t))
(add-hook 'prog-mode-hook 'komitee/rainbow-hook)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;; make
(defun komitee/tabs ()
  (setq indent-tabs-mode t))
(add-hook 'makefile-mode-hook 'komitee/tabs)

;; python
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args ""
      python-shell-prompt-regexp "In \\[[0-9]+\\]: "
      python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
      python-shell-completion-setup-code
      "from IPython.core.completerlib import module_completion"
      python-shell-completion-module-string-code
      "';'.join(module_completion('''%s'''))\n"
      python-shell-completion-string-code
      "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
